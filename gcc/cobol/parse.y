/*
 * Copyright (c) 2021-2025 Symas Corporation
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following disclaimer
 *   in the documentation and/or other materials provided with the
 *   distribution.
 * * Neither the name of the Symas Corporation nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
%code requires {
  #include <fstream>  // Before cobol-system because it uses poisoned functions
  #include "cobol-system.h"
  #include "coretypes.h"
  #include "../../libgcobol/io.h"
  #include "../../libgcobol/ec.h"
  #include "tree.h"

#pragma GCC diagnostic ignored "-Wmissing-field-initializers"

  enum radix_t {
    decimal_e = 10,
    hexadecimal_e = 16,
    boolean_e = 2,
  };

  enum accept_func_t {
    accept_done_e,
    accept_command_line_e,
    accept_envar_e,
  };

  class literal_t {
    size_t isym;
  public:
    char prefix[3];
    size_t len;
    char *data;

    bool empty() const { return data == NULL; }
    size_t isymbol() const { return isym; }
    const char * symbol_name() const {
      return isym? cbl_field_of(symbol_at(isym))->name : "";
    }

    literal_t&
    set( size_t len, char *data, const char prefix[] ) {
      set_prefix(prefix, strlen(prefix));
      set_data(len, data);
      return *this;
    }

    literal_t&
    set( const cbl_field_t * field ) {
      assert(field->has_attr(constant_e));
      assert(is_literal(field));

      set_prefix( "", 0 );
      set_data( field->data.capacity,
                const_cast<char*>(field->data.initial),
                field_index(field) );
      return *this;
    }
    literal_t&
    set_data( size_t len, char *data, size_t isym = 0 ) {
      this->isym = isym;
      this->len = len;
      this->data = data;
      if( this->prefix[0] == 'Z' ) {
        this->data = new char[++this->len];
        auto p = std::copy(data, data + len, this->data);
        *p = '\0';
      }
      return *this;
    }
    literal_t&
    set_prefix( const char *input, size_t len ) {
      assert(len < sizeof(prefix));
      std::fill(prefix, prefix + sizeof(prefix), '\0');
      std::transform(input, input + len, prefix, toupper);
      return *this;
    }
    bool
    compatible_prefix( const literal_t& that ) const {
      if( prefix[0] != that.prefix[0] ) {
        return prefix[0] != 'N' && that.prefix[0] != 'N';
      }
      return true;
    }
  };

  struct acrc_t { // Abbreviated combined relation condition
    cbl_refer_t *term;
    relop_t op;
    bool invert;
    acrc_t& init( cbl_refer_t *term = NULL,
                  relop_t op = relop_t(-1),
                  bool invert = false )
    {
      this->term = term;
      this->op = op;
      this->invert = invert;
      return *this;
    }
    static acrc_t make( cbl_refer_t *term = NULL,
                        relop_t op = relop_t(-1),
                        bool invert = false )
    {
      acrc_t output;
      return output.init( term, op, invert );
    }
    relop_t relop_from( relop_t ante_op ) const {
      assert(ante_op != -1);
      return op != -1? op : ante_op;
    }
    bool is_relation_condition() const { return term && term->field; }
  };
  typedef std::list<acrc_t> acrcs_t;

  enum data_category_t { data_category_none,
                         data_category_all,
                         data_alphabetic_e,
                         data_alphanumeric_e,
                         data_alphanumeric_edited_e,
                         data_boolean_e,
                         data_data_pointer_e,
                         data_function_pointer_e,
                         data_msg_tag_e,
                         data_dbcs_e,
                         data_egcs_e,
                         data_national_e,
                         data_national_edited_e,
                         data_numeric_e,
                         data_numeric_edited_e,
                         data_object_referenc_e,
                         data_program_pointer_e,
                       };

  const char * data_category_str( data_category_t category );

  typedef std::map<data_category_t, struct cbl_refer_t*> category_map_t;

  struct substitution_t {
    enum subst_fl_t { subst_all_e, subst_first_e = 'F', subst_last_e = 'L' };
    bool anycase;
    subst_fl_t first_last;
    cbl_refer_t *orig, *replacement;

    substitution_t& init( bool anycase, char first_last,
                            cbl_refer_t *orig, cbl_refer_t *replacement ) {
      this->anycase = anycase;
      switch(first_last) {
      case 'F': this->first_last = subst_first_e; break;
      case 'L': this->first_last = subst_last_e;  break;
      default:
        this->first_last = subst_all_e;
        break;
      }
      this->orig = orig;
      this->replacement = replacement;
      return *this;
    }
  };
  typedef std::list<substitution_t> substitutions_t;

  struct init_statement_t {
    bool to_value;
    data_category_t category;
    category_map_t replacement;

    init_statement_t( category_map_t replacement )
      : to_value(false)
      , category(data_category_none)
      , replacement(replacement)

    {}

    init_statement_t( bool to_value = false )
      : to_value(to_value)
      , category(data_category_none)
      , replacement(category_map_t())
    {}

  };

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
  static data_category_t
  data_category_of( const cbl_refer_t& refer );

  static REAL_VALUE_TYPE
  numstr2i( const char input[], radix_t radix );

  struct cbl_field_t;
  static inline cbl_field_t *
  new_literal( const char initial[], enum radix_t radix );
#pragma GCC diagnostic pop

  enum select_clause_t {
    access_clause_e        = 0x0001,
    alt_key_clause_e       = 0x0002,
    assign_clause_e        = 0x0004,
    collating_clause_e     = 0x0008,
    file_status_clause_e   = 0x0010,
    lock_mode_clause_e     = 0x0020,
    organization_clause_e  = 0x0040,
    padding_clause_e       = 0x0080,
    record_delim_clause_e  = 0x0100,
    record_key_clause_e    = 0x0200,
    relative_key_clause_e  = 0x0400,
    reserve_clause_e       = 0x0800,
    sharing_clause_e       = 0x1000,
  };

  struct symbol_elem_t;
  struct symbol_elem_t * symbols_begin( size_t first );
  struct symbol_elem_t * symbols_end();

  void field_done();

  template <typename E>
  struct Elem_list_t {
    std::list<E> elems;
    Elem_list_t() {}
    Elem_list_t( E elem ) {
      elems.push_back(elem);
    }
    Elem_list_t * push_back( E elem ) {
      elems.push_back(elem);
      return this;
    }
    void clear() {
      for( auto p = elems.begin(); p != elems.end(); p++ ) {
        assert( !(symbols_begin(0) <= *p && *p < symbols_end()) );
        delete *p;
      }
      elems.clear();
    }
  };

  struct file_list_t;
  struct cbl_label_t;
  typedef struct Elem_list_t<cbl_label_t*> Label_list_t;

  struct cbl_file_key_t;
  typedef struct Elem_list_t<cbl_file_key_t*> key_list_t;

  struct cbl_declarative_t;
  typedef struct Elem_list_t<cbl_declarative_t*> declarative_list_t;
  typedef struct Elem_list_t<ec_type_t> ec_list_t;
  typedef struct Elem_list_t<size_t> isym_list_t;

  struct rel_part_t;

  bool set_debug(bool);

#include "../../libgcobol/ec.h"
#include "../../libgcobol/common-defs.h"
#include "inspect.h"
}

%{
#include <fstream>  // Before cobol-system because it uses poisoned functions
#include "cobol-system.h"
#include "coretypes.h"
#include "tree.h"
#undef cobol_dialect
#undef cobol_exceptions
#undef yy_flex_debug
#include "cdfval.h"
#include "../../libgcobol/ec.h"
#include "../../libgcobol/common-defs.h"
#include "util.h"
#include "cbldiag.h"
#include "symbols.h"
#include "inspect.h"
#include "../../libgcobol/io.h"
#include "genapi.h"
#include "../../libgcobol/exceptl.h"
#include "exceptg.h"
#include "parse_ante.h"
%}

%token                  IDENTIFICATION_DIV   "IDENTIFICATION DIVISION"
			ENVIRONMENT_DIV      "ENVIRONMENT DIVISION"
			PROCEDURE_DIV        "PROCEDURE DIVISION"
                        DATA_DIV             "DATA DIVISION"
			FILE_SECT            "FILE SECTION"
			INPUT_OUTPUT_SECT    "INPUT-OUTPUT SECTION"
			LINKAGE_SECT         "LINKAGE SECTION"
                        LOCAL_STORAGE_SECT   "LOCAL-STORAGE SECTION"
			WORKING_STORAGE_SECT "WORKING-STORAGE SECTION"

%token                  OBJECT_COMPUTER "OBJECT COMPUTER"

%token                  DISPLAY_OF     "DISPLAY OF"
                        END_FUNCTION   "END FUNCTION"
			END_PROGRAM    "END PROGRAM"
			END_SUBPROGRAM "END PROGRAM <contained program>"

%token			JUSTIFIED RETURNING NO_CONDITION "invalid token"

%token  <string>        ALNUM ALPHED
%token  <number>        ERROR EXCEPTION SIZE_ERROR "SIZE ERROR"
%token	<ec_type>	EXCEPTION_NAME "EXCEPTION NAME"
%token  <number>        LEVEL LEVEL66 "66" LEVEL78 "78" LEVEL88 "88"
%token  <string>        CLASS_NAME "class name"
			NAME
			NAME88    "Level 88 NAME"
			NUME      "Name"
			NUMED     "NUMERIC-EDITED picture"
			NUMED_CR  "NUMERIC-EDITED CR picture"
			NUMED_DB  "NUMERIC-EDITED DB picture"
%token  <number>        NINEDOT NINES NINEV PIC_P
%token  <string>        SPACES
%token  <literal>       LITERAL
%token  <number>        END EOP
%token  <string>        FILENAME
%token  <number>        INVALID
%token  <number>        NUMBER NEGATIVE
%token  <numstr>        NUMSTR    "numeric literal"
%token  <number>        OVERFLOW_kw "OVERFLOW"
%token  <computational> COMPUTATIONAL

%token  <boolean>       PERFORM BACKWARD
%token  <number>        POSITIVE
%token  <field_attr>	POINTER
%token  <string>        SECTION
%token  <number>        STANDARD_ALPHABET "STANDARD ALPHABET"
%token  <string>        SWITCH
%token  <string>        UPSI
%token  <number>        ZERO

                         /* environment names */
%token  <number>        SYSIN SYSIPT SYSOUT SYSLIST SYSLST SYSPUNCH SYSPCH CONSOLE
%token  <number>        C01 C02 C03 C04 C05 C06 C07 C08 C09 C10 C11 C12 CSP
%token  <number>        S01 S02 S03 S04 S05 AFP_5A "AFP 5A"
%token  <number>        STDIN STDOUT STDERR

                         /* intrinsics */
%token  <string>        LIST MAP NOLIST NOMAP NOSOURCE
%token  <number>        MIGHT_BE "IS or IS NOT"
			FUNCTION_UDF   "UDF name"
			FUNCTION_UDF_0 "UDF"

%token  <string>        DATE_FMT "date format"
			TIME_FMT "time format"
			DATETIME_FMT "datetime format"

			/* tokens without semantic value */
                        /* CDF (COPY and >> defined here but used in cdf.y) */
%token			BASIS CBL CONSTANT COPY
			DEFINED ENTER FEATURE INSERTT
			LSUB "("
			PARAMETER_kw "PARAMETER"
			OVERRIDE READY RESET
			RSUB ")"
			SERVICE_RELOAD "SERVICE RELOAD" STAR_CBL "*CBL"
			SUBSCRIPT SUPPRESS TITLE TRACE USE

			COBOL_WORDS ">>COBOL-WORDS" EQUATE UNDEFINE
			CDF_DEFINE ">>DEFINE" CDF_DISPLAY ">>DISPLAY"
			CDF_IF ">>IF" CDF_ELSE ">>ELSE" CDF_END_IF ">>END-IF"
			CDF_EVALUATE ">>EVALUATE"
			CDF_WHEN ">>WHEN"
			CDF_END_EVALUATE ">>END-EVALUATE"
			CALL_COBOL "CALL" CALL_VERBATIM "CALL (as C)"

			IF THEN ELSE
			SENTENCE
			ACCEPT ADD ALTER CALL CANCEL CLOSE COMPUTE CONTINUE
			DELETE DISPLAY DIVIDE EVALUATE EXIT FILLER_kw "FILLER"
			GOBACK GOTO
			INITIALIZE INSPECT
			MERGE MOVE MULTIPLY OPEN PARAGRAPH
			READ RELEASE RETURN REWRITE
			SEARCH SET SELECT SORT SORT_MERGE "SORT-MERGE"
			STRING_kw "STRING" STOP SUBTRACT START
			UNSTRING WRITE WHEN

			   ABS ACCESS ACOS ACTUAL ADVANCING AFTER ALL
			ALLOCATE
			ALPHABET ALPHABETIC ALPHABETIC_LOWER "ALPHABETIC-LOWER"
			ALPHABETIC_UPPER "ALPHABETIC-UPPER"
			ALPHANUMERIC
			ALPHANUMERIC_EDITED "ALPHANUMERIC-EDITED"
			ALSO ALTERNATE ANNUITY ANUM ANY ANYCASE APPLY ARE
			AREA AREAS AS
			ASCENDING ACTIVATING ASIN ASSIGN AT ATAN

			BASED BASECONVERT
			BEFORE BINARY BIT BIT_OF "BIT-OF" BIT_TO_CHAR "BIT-TO-CHAR"
			BLANK BLOCK_kw
			BOOLEAN_OF_INTEGER "BOOLEAN-OF-INTEGER"
			BOTTOM BY
			BYTE BYTE_LENGTH "BYTE-LENGTH"

			CF CH
			CHANGED CHAR CHAR_NATIONAL "CHAR-NATIONAL"
			CHARACTER CHARACTERS CHECKING CLASS
			COBOL CODE CODESET COLLATING
			COLUMN COMBINED_DATETIME "COMBINED-DATETIME"
			COMMA COMMAND_LINE "COMMAND-LINE"
			COMMAND_LINE_COUNT "COMMAND-LINE-COUNT"
			COMMIT COMMON

			CONCAT CONDITION CONFIGURATION_SECT "CONFIGURATION SECTION"
			CONTAINS
			CONTENT CONTROL CONTROLS CONVERT CONVERTING CORRESPONDING COS
			COUNT CURRENCY CURRENT CURRENT_DATE

			DATA DATE DATE_COMPILED
			DATE_OF_INTEGER "DATE-OF-INTEGER"
			DATE_TO_YYYYMMDD "DATE-TO-YYYYMMDD"
			DATE_WRITTEN "DATE-WRITTEN"
			DAY DAY_OF_INTEGER "DAY-OF-INTEGER"
			DAY_OF_WEEK "DAY-OF-WEEK"
			DAY_TO_YYYYDDD "DAY-TO-YYYYDDD"
			DBCS DE DEBUGGING DECIMAL_POINT
			DECLARATIVES DEFAULT DELIMITED DELIMITER DEPENDING
			DESCENDING DETAIL DIRECT
			DIRECT_ACCESS "DIRECT-ACCESS"
			DOWN DUPLICATES
			DYNAMIC

			E EBCDIC EC EGCS ENTRY ENVIRONMENT EQUAL EVERY
			EXAMINE EXHIBIT EXP EXP10 EXTEND EXTERNAL

			EXCEPTION_FILE	     "EXCEPTION-FILE"
			EXCEPTION_FILE_N     "EXCEPTION-FILE-N"
			EXCEPTION_LOCATION   "EXCEPTION-LOCATION"
			EXCEPTION_LOCATION_N "EXCEPTION-LOCATION-N"
			EXCEPTION_STATEMENT  "EXCEPTION-STATEMENT"
			EXCEPTION_STATUS     "EXCEPTION-STATUS"

			FACTORIAL FALSE_kw "False" FD
			FILE_CONTROL "FILE-CONTROL"
			FILE_KW "File"
			FILE_LIMIT "FILE-LIMIT"
			FINAL FINALLY
			FIND_STRING "FIND-STRING"
			FIRST FIXED FOOTING FOR
			FORMATTED_CURRENT_DATE "FORMATTED-CURRENT-DATE"
			FORMATTED_DATE "FORMATTED-DATE"
			FORMATTED_DATETIME "FORMATTED-DATETIME"
			FORMATTED_TIME "FORMATTED-TIME"
			FORM_OVERFLOW "FORM-OVERFLOW"
			FREE
			FRACTION_PART "FRACTION-PART"
			FROM FUNCTION

			GENERATE GIVING GLOBAL GO GROUP

			HEADING HEX
			HEX_OF "HEX-OF"
			HEX_TO_CHAR "HEX-TO-CHAR"
			HIGH_VALUES "HIGH-VALUES"
			HIGHEST_ALGEBRAIC "HIGHEST-ALGEBRAIC"
			HOLD

			IBM_360 IN INCLUDE INDEX INDEXED INDICATE INITIAL_kw "INITIAL"
			INITIATE INPUT INSTALLATION INTERFACE
			INTEGER
			INTEGER_OF_BOOLEAN "INTEGER-OF-BOOLEAN"
			INTEGER_OF_DATE "INTEGER-OF-DATE"
			INTEGER_OF_DAY "INTEGER-OF-DAY"
			INTEGER_OF_FORMATTED_DATE "INTEGER-OF-FORMATTED-DATE"
			INTEGER_PART "INTEGER-PART"
			INTO INTRINSIC INVOKE IO IO_CONTROL "IO-CONTROL"
			IS ISNT "IS NOT"

			KANJI KEY

			LABEL LAST LEADING LEFT LENGTH
			LENGTH_OF "LENGTH-OF"
			LIMIT LIMITS LINE LINES
			LINE_COUNTER "LINE-COUNTER"
			LINAGE LINKAGE LOCALE LOCALE_COMPARE "LOCALE-COMPARE"
			LOCALE_DATE "LOCALE-DATE"
			LOCALE_TIME "LOCALE-TIME"
			LOCALE_TIME_FROM_SECONDS "LOCALE-TIME-FROM-SECONDS"
			LOCAL_STORAGE "LOCAL-STORAGE"
			LOCATION
			LOCK LOCK_ON LOG LOG10
			LOWER_CASE "LOWER-CASE"
			LOW_VALUES "LOW-VALUES"
			LOWEST_ALGEBRAIC "LOWEST-ALGEBRAIC"
			LPAREN " )"

			MANUAL MAXX "Max" MEAN MEDIAN MIDRANGE
			MINN "Min" MULTIPLE MOD MODE
			MODULE_NAME  "MODULE-NAME "

			NAMED NAT NATIONAL
			NATIONAL_EDITED "NATIONAL-EDITED"
			NATIONAL_OF "NATIONAL-OF"
			NATIVE NESTED NEXT
			NO NOTE
			NULLS NULLPTR
			NUMERIC
			NUMERIC_EDITED NUMVAL
			NUMVAL_C "NUMVAL-C"
			NUMVAL_F "NUMVAL-F"

			OCCURS OF OFF OMITTED ON ONLY OPTIONAL OPTIONS ORD ORDER
			ORD_MAX "ORD-MAX"
			ORD_MIN "ORD-MIN"
			ORGANIZATION OTHER OTHERWISE OUTPUT

			PACKED_DECIMAL PADDING PAGE
			PAGE_COUNTER "PAGE-COUNTER"
			PF PH PI PIC PICTURE
			PLUS PRESENT_VALUE PRINT_SWITCH
			PROCEDURE PROCEDURES PROCEED PROCESS
			PROGRAM_ID "PROGRAM-ID"
			PROGRAM_kw "Program" PROPERTY PROTOTYPE PSEUDOTEXT

			QUOTES "QUOTE"

			RANDOM RANDOM_SEED RANGE RAISE RAISING
			RD RECORD RECORDING RECORDS RECURSIVE
			REDEFINES REEL REFERENCE RELATIVE REM REMAINDER REMARKS
			REMOVAL RENAMES	 REPLACE REPLACING REPORT REPORTING REPORTS
			REPOSITORY RERUN RESERVE RESTRICTED RESUME
			REVERSE REVERSED REWIND RF RH RIGHT ROUNDED RUN

			SAME SCREEN SD
			SECONDS_FROM_FORMATTED_TIME "SECONDS-FROM-FORMATTED-TIME"
			SECONDS_PAST_MIDNIGHT "SECONDS-PAST-MIDNIGHT"
			SECURITY
			SEPARATE SEQUENCE SEQUENTIAL SHARING
			SIMPLE_EXIT "(simple) EXIT"
			SIGN SIN SIZE
			SMALLEST_ALGEBRAIC "SMALLEST-ALGEBRAIC"
			SOURCE
			SOURCE_COMPUTER "SOURCE-COMPUTER"
			SPECIAL_NAMES SQRT STACK
			STANDARD
			STANDARD_1 "STANDARD-1"
			STANDARD_DEVIATION  "STANDARD-DEVIATION "
			STANDARD_COMPARE "STANDARD-COMPARE"
			STATUS STRONG
			SUBSTITUTE SUM SYMBOL SYMBOLIC SYNCHRONIZED

			TALLY TALLYING TAN TERMINATE TEST
			TEST_DATE_YYYYMMDD "TEST-DATE-YYYYMMDD"
			TEST_DAY_YYYYDDD "TEST-DAY-YYYYDDD"
			TEST_FORMATTED_DATETIME "TEST-FORMATTED-DATETIME"
			TEST_NUMVAL "TEST-NUMVAL"
			TEST_NUMVAL_C "TEST-NUMVAL-C"
			TEST_NUMVAL_F "TEST-NUMVAL-F"
			THAN TIME TIMES
			TO TOP
			TOP_LEVEL
			TRACKS TRACK_AREA TRAILING TRANSFORM TRIM TRUE_kw "True" TRY
			TURN TYPE TYPEDEF

			ULENGTH UNBOUNDED UNIT UNITS UNIT_RECORD UNTIL UP UPON
			UPOS UPPER_CASE USAGE USING USUBSTR USUPPLEMENTARY
			UTILITY UUID4 UVALID UWIDTH

			VALUE VARIANCE VARYING VOLATILE

			WHEN_COMPILED WITH WORKING_STORAGE
			XML XMLGENERATE XMLPARSE
			YEAR_TO_YYYY YYYYDDD YYYYMMDD

			/* unused Context Words */
			ARITHMETIC ATTRIBUTE AUTO AUTOMATIC
			AWAY_FROM_ZERO "AWAY-FROM-ZERO"
			BACKGROUND_COLOR "BACKGROUND-COLOR"
			BELL
			BINARY_ENCODING "BINARY-ENCODING"
			BLINK
			CAPACITY CENTER CLASSIFICATION CYCLE
			DECIMAL_ENCODING "DECIMAL-ENCODING"
			ENTRY_CONVENTION EOL EOS ERASE EXPANDS
			FLOAT_BINARY "FLOAT-BINARY"
			FLOAT_DECIMAL "FLOAT-DECIMAL"
			FOREGROUND_COLOR FOREVER FULL
			HIGHLIGHT
			HIGH_ORDER_LEFT "HIGH-ORDER-LEFT"
			HIGH_ORDER_RIGHT "HIGH-ORDER-RIGHT"
			IGNORING IMPLEMENTS INITIALIZED INTERMEDIATE
			LC_ALL_kw	"LC-ALL"
			LC_COLLATE_kw	"LC-COLLATE"
			LC_CTYPE_kw	"LC-CTYPE"
			LC_MESSAGES_kw	"LC-MESSAGES"
			LC_MONETARY_kw	"LC-MONETARY"
			LC_NUMERIC_kw	"LC-NUMERIC"
			LC_TIME_kw	"LC-TIME"
			LOWLIGHT
			NEAREST_AWAY_FROM_ZERO "NEAREST-AWAY-FROM-ZERO"
			NEAREST_EVEN NEAREST_TOWARD_ZERO "NEAREST-EVEN NEAREST-TOWARD-ZERO"
			NONE NORMAL NUMBERS
			PREFIXED PREVIOUS PROHIBITED RELATION REQUIRED
			REVERSE_VIDEO ROUNDING
			SECONDS SECURE SHORT SIGNED_kw
			STANDARD_BINARY "STANDARD-BINARY"
			STANDARD_DECIMAL "STANDARD-DECIMAL"
			STATEMENT STEP STRUCTURE
			TOWARD_GREATER "TOWARD-GREATER"
			TOWARD_LESSER "TOWARD-LESSER"
			TRUNCATION
			UCS_4 "UCS-4"
			UNDERLINE UNSIGNED_kw
			UTF_16 "UTF-16"
			UTF_8 "UTF-8"

			ADDRESS
			END_ACCEPT "END-ACCEPT"
			END_ADD "END-ADD"
			END_CALL "END-CALL"
			END_COMPUTE "END-COMPUTE"
			END_DELETE "END-DELETE"
			END_DISPLAY "END-DISPLAY"
			END_DIVIDE "END-DIVIDE"
			END_EVALUATE "END-EVALUATE"
			END_MULTIPLY "END-MULTIPLY"
			END_PERFORM "END-PERFORM"
			END_READ "END-READ"
			END_RETURN "END-RETURN"
			END_REWRITE "END-REWRITE"
			END_SEARCH "END-SEARCH"
			END_START "END-START"
			END_STRING "END-STRING"
			END_SUBTRACT "END-SUBTRACT"
			END_UNSTRING "END-UNSTRING"
			END_WRITE "END-WRITE"
			END_IF "END-IF"
			/* end tokens without semantic value */

    // YYEOF added for compatibility with Bison 3.5
    // https://savannah.gnu.org/forum/forum.php?forum_id=9735
%token  YYEOF 0 "end of file"

%type   <number>        sentence statements statement
%type   <number>        star_cbl_opt close_how

%type   <number>        test_before usage_clause1 might_be
%type   <boolean>       all optional sign_leading on_off initialized strong
%type   <number>        count data_clauses data_clause
%type   <number>        nine nines nps relop spaces_etc reserved_value signed
%type   <number>        variable_type
%type   <number>        true_false posneg eval_posneg
%type   <number>        open_io alphabet_etc
%type   <special_type>  device_name
%type   <string>        numed  collating_sequence context_word ctx_name locale_spec
%type   <literal>       namestr alphabet_lit program_as repo_as
%type   <field>         perform_cond kind_of_name
%type   <refer>         alloc_ret

%type	<field>		log_term rel_expr rel_abbr eval_abbr
%type   <refer>		num_value num_term value factor
%type   <refer>         simple_cond bool_expr
%type	<log_expr_t>	log_expr rel_abbrs eval_abbrs
%type   <rel_term_t>	rel_term rel_term1

%type   <field_data>    value78
%type   <field>         literal name nume typename
%type   <field>         num_literal signed_literal

%type	<number>	perform_start
%type   <refer>         perform_times
%type   <perf>          perform_verb
			perform_inline perform_except

%type   <refer>         eval_subject1
%type   <vargs>         vargs disp_vargs;
%type   <field>         level_name
%type   <string>        fd_name picture_sym name66 paragraph_name
%type   <literal>       literalism
%type   <number>        bound advance_when org_clause1 read_next
%type   <number>        access_mode multiple lock_how lock_mode
%type   <select_clauses> select_clauses
%type   <select_clause> select_clause  access_clause alt_key_clause
                        assign_clause collate_clause status_clause
                        lock_mode_clause org_clause padding_clause
                        record_delim_clause record_key_clause
                        relative_key_clause reserve_clause sharing_clause

%type   <file>          filename read_body write_body delete_body
%type   <rewrite_t>     rewrite_body
%type   <min_max>       record_vary rec_contains from_to record_desc
%type   <file_op>       read_file rewrite1 write_file
%type   <field>         data_descr data_descr1 write_what file_record
%type   <field>         name88
%type   <refer>         advancing  advance_by
%type   <refer>         alphaval alpha_val numeref scalar scalar88
%type   <refer>         tableref tableish
%type   <refer>         varg varg1 varg1a
%type   <refer>         expr expr_term compute_expr free_tgt by_value_arg
%type   <refer>         move_tgt selected_name read_key read_into vary_by
%type   <refer>         accept_refer num_operand envar search_expr any_arg
%type   <accept_func>	accept_body
%type   <refers>        expr_list subscripts arg_list free_tgts
%type   <targets>       move_tgts set_tgts
%type   <field>         search_varying
%type   <field>         search_term search_terms
%type   <label>         label_name
%type   <tgt>           sort_target
%type   <files>         filenames cdf_use_files
%type   <field>         one_switch
%type   <fields>        field_list switches key_sources key_source
%type   <sort_keys>     sort_keys
%type   <sort_key>      sort_key
%type   <sort_io>       sort_input sort_output
%type   <boolean>       sort_dup forward_order unique_key sign_separate
%type   <number>        befter cardinal initial first_leading

%type   <refer>         inspected
%type   <insp_qual>     insp_qual
%type   <insp_match>    insp_quals insp_mtquals tally_match
%type   <insp_replace>  x_by_y
%type   <insp_oper>     replace_oper x_by_ys
%type   <insp_oper>     tally_forth tally_matches
%type   <inspect>       tally
%type   <insp_one>      replacement tally_fors
%type   <insp_all>      tallies replacements

%type   <arith>         add_body subtract_body multiply_body divide_body
%type   <arith>         add_impl subtract_impl multiply_impl divide_impl
%type   <arith>         add_cond subtract_cond multiply_cond divide_cond
%type   <arith>         divide_into divide_by

%type   <refer>         intrinsic_call
%type   <field>         intrinsic intrinsic_locale

%type   <field>         intrinsic0
%type   <number>        intrinsic_v intrinsic_I intrinsic_N intrinsic_X
%type   <number>        intrinsic_I2 intrinsic_N2 intrinsic_X2
%type   <number>        lopper_case
%type   <number>        return_body return_file
%type   <field>         trim_trailing function_udf

%type   <refer>         str_input str_size
%type   <refer2>        str_into

%type   <refers>        sum  scalar88s ffi_names
%type   <delimited_1>   str_delimited
%type   <delimiteds>    str_delimiteds
%type   <str_body>      string_body

%type   <refmod_parts>  refmod

%type   <uns_body>      unstring_body
%type   <refers>        uns_delimiters uns_delimited
%type   <refer>         uns_delimiter
%type   <uns_into>      uns_into
%type   <uns_tgts>      uns_tgts
%type   <uns_tgt>       uns_tgt

%type   <error>         on_overflow on_overflows
%type   <error>         arith_err arith_errs
%type   <error>         accept_except accept_excepts call_except call_excepts
%type   <compute_body_t> compute_body

%type   <refer>         ffi_name set_operand set_tgt scalar_arg unstring_src
%type   <number>        /* addr_len_of */ alphanum_pic
%type   <pic_part>      alphanum_part

%type   <ffi_arg>       parameter ffi_by_ref ffi_by_con ffi_by_val
%type   <ffi_args>      parameters
%type   <ffi_impl>      call_body call_impl

%type   <ffi_arg>       procedure_use
%type   <ffi_args>      procedure_uses

%type   <comminit>      comminit comminits program_attrs

%type   <error_clauses> io_invalids read_eofs write_eops
%type   <boolean>       io_invalid  read_eof  write_eop
                        global is_global anycase backward
%type   <number>        mistake globally first_last
%type   <io_mode>   io_mode

%type   <labels>        labels
%type   <label>         label_1 section_name

%type   <switches>      upsi_entry

%type   <special>       acceptable disp_target
%type   <display>       disp_body

%type   <false_domain>  domains domain
%type   <colseq>        alphabet_seq
%type   <alphasym>      alphabet_name alphabet_seqs sort_seq

%type   <init_stmt>     init_clause init_value
%type   <data_category> init_categora init_category
%type   <replacement>   init_by
%type   <replacements>  init_bys init_replace
%type   <refer>         init_data exit_with stop_status
%type   <float128>      cce_expr cce_factor const_value
%type   <prog_end>      end_program1
%type   <substitution>  subst_input
%type   <substitutions> subst_inputs
%type   <numval_locale_t> numval_locale

%type	<ec_type>	except_name exit_raising
%type	<ec_list>	except_names
%type	<isym_list>	except_files
%type	<dcl_list_t>	perform_ec

%type	<opt_init_sects>	opt_init_sects
%type	<opt_init_sect>	        opt_init_sect
%type	<number>        opt_init_value
%type	<opt_round>	rounded round_between rounded_type rounded_mode
%type	<opt_arith>	opt_arith_type
%type	<module_type>	module_type

%union {
    bool boolean;
    int number;
    char *string;
    REAL_VALUE_TYPE float128;
    literal_t literal;
    cbl_field_attr_t field_attr;
    ec_type_t ec_type;
    ec_list_t* ec_list;
           declarative_list_t* dcl_list_t;
           isym_list_t* isym_list;
    struct { radix_t radix; char *string; } numstr;
    struct { int token; literal_t name; } prog_end;
    struct { int token; special_name_t id; } special_type;
    struct { cbl_field_type_t type;
             uint32_t capacity; bool signable; } computational;
    struct cbl_special_name_t *special;
    struct cbl_alphabet_t *alphasym;
    struct tgt_list_t *targets;
    struct cbl_file_t *file;
    struct { bool varying; size_t min, max; } min_max;
    struct { cbl_file_t *file; cbl_field_t *buffer; } rewrite_t;
    struct { cbl_file_t *file; file_status_t handled; } file_op;
    struct cbl_label_t *label;
    struct { cbl_label_t *label; int token; } exception;
    struct cbl_field_data_t *field_data;
    struct cbl_field_t *field;
    struct { bool tf; cbl_field_t *field; } bool_field;
    struct { int token; cbl_field_t *cond; } cond_field;
    struct cbl_refer_t *refer;

    struct rel_term_type { bool invert; cbl_refer_t *term; } rel_term_t;
    struct log_expr_t *log_expr_t;
    struct vargs_t* vargs;
    struct perform_t *perf;
    struct cbl_perform_tgt_t *tgt;
           Label_list_t *labels;
           key_list_t *file_keys;
           cbl_file_mode_t io_mode;
    struct cbl_file_key_t *file_key;
    struct file_list_t *files;
    struct field_list_t *fields;
    struct refer_list_t *refers;
    struct sort_key_t *sort_key;
    struct sort_keys_t *sort_keys;
    struct file_sort_io_t *sort_io;
    struct arith_t *arith;
    struct { size_t ntgt; cbl_num_result_t *tgts;
             cbl_refer_t *expr; } compute_body_t;
    struct ast_inspect_t *insp_one;
    struct ast_inspect_list_t *insp_all;
    struct ast_inspect_oper_t *insp_oper;
    struct { bool before; cbl_inspect_qual_t *qual; } insp_qual;
           cbl_inspect_t *inspect;
           cbl_inspect_match_t *insp_match;
           cbl_inspect_replace_t *insp_replace;

    struct { cbl_refer_t *delimited; refer_list_t *inputs; } delimited;
    struct { cbl_refer_t *input, *delimiter; } delimited_1;
    struct { cbl_refer_t *from, *len; } refmod_parts;
    struct refer_collection_t *delimiteds;
    struct { cbl_label_t *on_error, *not_error; } error;
    struct { unsigned int nclause; bool tf; } error_clauses;
    struct refer_pair_t { cbl_refer_t *first, *second; } refer2;
    struct { refer_collection_t *inputs; refer_pair_t into; } str_body;

    struct { accept_func_t func; cbl_refer_t *into, *from; } accept_func;
    struct unstring_into_t *uns_into;
    struct unstring_tgt_list_t *uns_tgts;
    struct unstring_tgt_t *uns_tgt;
    struct { cbl_refer_t *input;
             refer_list_t *delimited; unstring_into_t *into; } uns_body;

    struct cbl_ffi_arg_t *ffi_arg;
    struct ffi_args_t *ffi_args;
    struct { YYLTYPE loc; cbl_refer_t *ffi_name, *ffi_returning;
             ffi_args_t *using_params; } ffi_impl;

    struct { bool common, initial, recursive; } comminit;
    struct { enum select_clause_t clause; cbl_file_t *file; } select_clause;
    struct { size_t clauses; cbl_file_t *file; } select_clauses;
    struct { YYLTYPE loc; char *on, *off; } switches;
    struct cbl_domain_t *false_domain;
    struct { size_t also; unsigned char *low, *high; } colseq;
    struct { cbl_field_attr_t attr; int nbyte; } pic_part;

           data_category_t data_category;
    struct { data_category_t category; cbl_refer_t* replacement; } replacement;
           category_map_t *replacements;
           init_statement_t *init_stmt;
    struct { cbl_special_name_t *special; vargs_t *vargs; } display;
           substitution_t substitution;
           substitutions_t  *substitutions;
    struct { bool is_locale; cbl_refer_t *arg2; } numval_locale_t;

    cbl_options_t::arith_t opt_arith;
    cbl_round_t opt_round;
    cbl_section_type_t opt_init_sect;
    struct { bool local, working; } opt_init_sects;
    module_type_t module_type;
}

%printer { fprintf(yyo, "clauses: 0x%04x", $$); } data_clauses
%printer { fprintf(yyo, "%s %s", refer_type_str($$), $$? $$->name() : "<none>"); } <refer>
%printer { fprintf(yyo, "%s", $$->field? name_of($$->field) : "[omitted]"); } alloc_ret
%printer { fprintf(yyo, "%s %s '%s' (%s)",
                        $$? cbl_field_type_str($$->type) : "<%empty>",
                        $$? name_of($$) : "",
                        $$? $$->data.initial? $$->data.initial : "<nil>" : "",
                        $$? $$->value_str() : "" ); } <field>

%printer { fprintf(yyo, "%c %s",
                        $$.invert? '!' : ' ',
		        $$.term? name_of($$.term->field) : "<none>"); } <rel_term_t>

%printer { fprintf(yyo, "%s (token %d)", keyword_str($$), $$ ); } relop
%printer { fprintf(yyo, "'%s'", $$? $$ : "" ); } NAME <string>
%printer { fprintf(yyo, "%s'%.*s'{%zu} %s", $$.prefix, int($$.len), $$.data, $$.len,
                        $$.symbol_name()); } <literal>
%printer { fprintf(yyo, "%s (1st of %zu)",
                        $$->targets.empty()? "" : $$->targets.front().refer.field->name,
                        $$->targets.size() ); } <targets>
%printer { fprintf(yyo, "#%zu: %s",
                        is_temporary($$)? 0 : field_index($$),
                        $$? name_of($$) : "<nil>" ); } name
%printer { fprintf(yyo, "{%zu-%zu}", $$.min, $$.max ); } <min_max>
%printer { fprintf(yyo, "{%s}", $$? "+/-" : "" ); } signed
%printer { fprintf(yyo, "{%s of %zu}",
		         teed_up_names().front(), teed_up_names().size() ); } qname
%printer { fprintf(yyo, "{%d}", $$ ); } <number>
%printer { fprintf(yyo, "'%s'", $$.string ); } <numstr>
%printer { const char *s = string_of($$);
           fprintf(yyo, "{%s}", s? s : "??" ); } <float128>
%printer { fprintf(yyo, "{%s %c%u}", cbl_field_type_str($$.type),
                                     $$.signable? '+' : ' ',
                                     $$.capacity ); } <computational>
%printer { fprintf(yyo, "{'%s'-'%s'%s}",
                        $$.low?  (const char*) $$.low : "",
                        $$.high? (const char*) $$.high : "",
                        $$.also? "+" : "" ); } <colseq>
%printer { fprintf(yyo, "{%s, %zu parameters}",
                        name_of($$.ffi_name->field), !$$.using_params? 0 :
                        $$.using_params->elems.size()); } call_body
%printer { fprintf(yyo, "%s <- %s", data_category_str($$.category),
                                    name_of($$.replacement->field)); } init_by

                        /* CDF (COPY and >> defined here but used in cdf.y) */
%left                   BASIS CBL CONSTANT COPY
                        DEFINED ENTER FEATURE INSERTT
                        LIST LSUB MAP NOLIST NOMAP NOSOURCE
                        PARAMETER_kw OVERRIDE READY RESET RSUB
                        SERVICE_RELOAD STAR_CBL
                        SUBSCRIPT SUPPRESS TITLE TRACE USE

			COBOL_WORDS EQUATE UNDEFINE

                        CDF_DEFINE CDF_DISPLAY
                        CDF_IF CDF_ELSE CDF_END_IF
                        CDF_EVALUATE
			CDF_WHEN
			CDF_END_EVALUATE
                        CALL_COBOL CALL_VERBATIM

%right                  IF THEN ELSE
                        SENTENCE
                        ACCEPT ADD ALTER CALL CANCEL CLOSE COMPUTE CONTINUE
                        DELETE DISPLAY DIVIDE EVALUATE END EOP EXIT FILLER_kw
                        GOBACK GOTO
                        INITIALIZE INSPECT
                        MERGE MOVE MULTIPLY OPEN OVERFLOW_kw PARAGRAPH PERFORM
                        READ RELEASE RETURN REWRITE
                        SEARCH SET SELECT SORT SORT_MERGE
                        STRING_kw STOP SUBTRACT START
                        UNSTRING WRITE WHEN INVALID

%left                   ABS ACCESS ACOS ACTUAL ADVANCING AFP_5A AFTER ALL
                        ALLOCATE
                        ALPHABET ALPHABETIC ALPHABETIC_LOWER
                        ALPHABETIC_UPPER
			ALPHANUMERIC
			ALPHANUMERIC_EDITED
                        ALPHED ALSO ALTERNATE ANNUITY ANUM ANY ANYCASE APPLY ARE
                        AREA AREAS AS
                        ASCENDING ACTIVATING ASIN ASSIGN AT ATAN

                        BACKWARD BASED BASECONVERT
			BEFORE BINARY BIT BIT_OF BIT_TO_CHAR
                        BLANK BLOCK_kw
			BOOLEAN_OF_INTEGER
			BOTTOM BY
			BYTE BYTE_LENGTH

                        C01 C02 C03 C04 C05 C06 C07 C08 C09 C10 C11 C12 CF CH
                        CHANGED CHAR CHAR_NATIONAL
			CHARACTER CHARACTERS CHECKING CLASS
                        COBOL CODE CODESET COLLATING
                        COLUMN COMBINED_DATETIME
			COMMA COMMAND_LINE
                        COMMAND_LINE_COUNT
			COMMIT COMMON COMPUTATIONAL

                        CONCAT CONDITION CONFIGURATION_SECT
			CONSOLE CONTAINS
                        CONTENT CONTROL CONTROLS CONVERT CONVERTING CORRESPONDING COS
                        COUNT CSP CURRENCY CURRENT CURRENT_DATE

                        DATA DATE DATE_COMPILED
			DATE_OF_INTEGER
			DATE_TO_YYYYMMDD
                        DATE_FMT
			TIME_FMT
			DATETIME_FMT
                        DATE_WRITTEN
			DAY DAY_OF_INTEGER
			DAY_OF_WEEK
                        DAY_TO_YYYYDDD
			DBCS DE DEBUGGING DECIMAL_POINT
                        DECLARATIVES DEFAULT DELIMITED DELIMITER DEPENDING
                        DESCENDING DETAIL DIRECT
			DIRECT_ACCESS
			DOWN DUPLICATES
                        DYNAMIC

                        E EBCDIC EC EGCS ENTRY ENVIRONMENT EQUAL ERROR EVERY
                        EXAMINE EXCEPTION EXHIBIT EXP EXP10 EXTEND EXTERNAL

                        EXCEPTION_FILE
			EXCEPTION_FILE_N
                        EXCEPTION_LOCATION
			EXCEPTION_LOCATION_N
                        EXCEPTION_NAME
                        EXCEPTION_STATEMENT
			EXCEPTION_STATUS

                        FACTORIAL FALSE_kw FD FILENAME
			FILE_CONTROL
			FILE_KW
                        FILE_LIMIT
			FINAL FINALLY
			FIND_STRING
			FIRST FIXED FOOTING FOR
                        FORMATTED_CURRENT_DATE
			FORMATTED_DATE
			FORMATTED_DATETIME
                        FORMATTED_TIME
			FORM_OVERFLOW
			FREE
                        FRACTION_PART
			FROM FUNCTION
			FUNCTION_UDF

                        GENERATE GIVING GLOBAL GO GROUP

                        HEADING HEX
			HEX_OF
			HEX_TO_CHAR
                        HIGH_VALUES
			HIGHEST_ALGEBRAIC
			HOLD

                        IBM_360 IN INCLUDE INDEX INDEXED INDICATE INITIAL_kw
                        INITIATE INPUT INSTALLATION INTERFACE
                        INTEGER
			INTEGER_OF_BOOLEAN
			INTEGER_OF_DATE
                        INTEGER_OF_DAY
			INTEGER_OF_FORMATTED_DATE
			INTEGER_PART
                        INTO INTRINSIC INVOKE IO IO_CONTROL
			IS ISNT

                        KANJI KEY

                        LABEL LAST LEADING LEFT LENGTH
			LENGTH_OF
			LEVEL LEVEL66
                        LEVEL88 LIMIT LIMITS LINE LINES
			LINE_COUNTER
                        LINAGE LINKAGE LOCALE LOCALE_COMPARE
                        LOCALE_DATE
			LOCALE_TIME
			LOCALE_TIME_FROM_SECONDS
                        LOCAL_STORAGE
			LOCATION
                        LOCK LOCK_ON LOG LOG10
			LOWER_CASE
			LOW_VALUES
                        LOWEST_ALGEBRAIC
			LPAREN

                        MANUAL MAXX MEAN MEDIAN MIDRANGE
                        MIGHT_BE MINN MULTIPLE MOD MODE
			MODULE_NAME

                        NAMED NAT NATIONAL
			NATIONAL_EDITED
			NATIONAL_OF
                        NATIVE NEGATIVE NESTED NEXT
			NINEDOT NINES NINEV NO NOTE NO_CONDITION
			NULLS NULLPTR NUMBER
                        NUME NUMED NUMED_CR NUMED_DB NUMERIC
                        NUMERIC_EDITED NUMSTR NUMVAL
			NUMVAL_C
			NUMVAL_F

                        OCCURS OF OFF OMITTED ON ONLY OPTIONAL OPTIONS ORD ORDER
                        ORD_MAX
			ORD_MIN
			ORGANIZATION OTHER OTHERWISE OUTPUT

                        PACKED_DECIMAL PADDING PAGE
			PAGE_COUNTER
                        PF PH PI PIC PICTURE PIC_P
                        PLUS POINTER POSITIVE PRESENT_VALUE PRINT_SWITCH
                        PROCEDURE PROCEDURES PROCEED PROCESS
                        PROGRAM_ID
			PROGRAM_kw PROPERTY PROTOTYPE PSEUDOTEXT

                        QUOTES

                        RANDOM RANDOM_SEED RANGE RAISE RAISING
                        RD RECORD RECORDING RECORDS RECURSIVE
                        REDEFINES REEL REFERENCE RELATIVE REM REMAINDER REMARKS
                        REMOVAL RENAMES  REPLACE REPLACING REPORT REPORTING REPORTS
                        REPOSITORY RERUN RESERVE RESTRICTED RESUME
                        REVERSE REVERSED REWIND RF RH RIGHT ROUNDED RUN

                        S01 S02 S03 S04 S05 SAME SCREEN SD
			SECONDS_FROM_FORMATTED_TIME
                        SECONDS_PAST_MIDNIGHT
			SECTION SECURITY
                        SEPARATE SEQUENCE SEQUENTIAL SHARING
			SIMPLE_EXIT
                        SIGN SIN SIZE SIZE_ERROR
			SMALLEST_ALGEBRAIC
			SOURCE
			SOURCE_COMPUTER
                        SPACES SPECIAL_NAMES SQRT STACK
			STANDARD
			STANDARD_ALPHABET
			STANDARD_1
			STANDARD_DEVIATION
                        STANDARD_COMPARE
			STATUS STRONG STDERR STDIN STDOUT
                        LITERAL SUBSTITUTE SUM SWITCH SYMBOL SYMBOLIC SYNCHRONIZED
                        SYSIN SYSIPT SYSLST SYSOUT SYSPCH SYSPUNCH

                        TALLY TALLYING TAN TERMINATE TEST
			TEST_DATE_YYYYMMDD
                        TEST_DAY_YYYYDDD
			TEST_FORMATTED_DATETIME
			TEST_NUMVAL
                        TEST_NUMVAL_C
			TEST_NUMVAL_F
			THAN TIME TIMES
			TO TOP
			TOP_LEVEL
                        TRACKS TRACK_AREA TRAILING TRANSFORM TRIM TRUE_kw TRY
                        TURN TYPE TYPEDEF

                        ULENGTH UNBOUNDED UNIT UNITS UNIT_RECORD UNTIL UP UPON
                        UPOS UPPER_CASE UPSI USAGE USING USUBSTR USUPPLEMENTARY
                        UTILITY UUID4 UVALID UWIDTH

                        VALUE VARIANCE VARYING VOLATILE

                        WHEN_COMPILED WITH WORKING_STORAGE
                        XML XMLGENERATE XMLPARSE
                        YEAR_TO_YYYY YYYYDDD YYYYMMDD
                        ZERO

                        /* unused Context Words */
                        ARITHMETIC ATTRIBUTE AUTO AUTOMATIC
			AWAY_FROM_ZERO
                        BACKGROUND_COLOR
			BELL
			BINARY_ENCODING
			BLINK
                        CAPACITY CENTER CLASSIFICATION CYCLE
                        DECIMAL_ENCODING
			ENTRY_CONVENTION EOL EOS ERASE EXPANDS
                        FLOAT_BINARY
			FLOAT_DECIMAL
			FOREGROUND_COLOR FOREVER FULL
                        HIGHLIGHT
			HIGH_ORDER_LEFT
			HIGH_ORDER_RIGHT
                        IGNORING IMPLEMENTS INITIALIZED INTERMEDIATE
                        LC_ALL_kw
			LC_COLLATE_kw
			LC_CTYPE_kw
			LC_MESSAGES_kw
                        LC_MONETARY_kw
			LC_NUMERIC_kw
			LC_TIME_kw
                        LOWLIGHT
                        NEAREST_AWAY_FROM_ZERO
			NEAREST_EVEN NEAREST_TOWARD_ZERO
                        NONE NORMAL NUMBERS
                        PREFIXED PREVIOUS PROHIBITED RELATION REQUIRED
                        REVERSE_VIDEO ROUNDING
                        SECONDS SECURE SHORT SIGNED_kw
			STANDARD_BINARY
                        STANDARD_DECIMAL
			STATEMENT STEP STRUCTURE
                        TOWARD_GREATER
			TOWARD_LESSER
			TRUNCATION
                        UCS_4
			UNDERLINE UNSIGNED_kw
			UTF_16
			UTF_8

%left                   CLASS_NAME NAME NAME88
%left                   ADDRESS
%left                   END_ACCEPT END_ADD END_CALL END_COMPUTE
			END_DELETE END_DISPLAY END_DIVIDE
			END_EVALUATE END_MULTIPLY END_PERFORM
			END_READ END_RETURN END_REWRITE
			END_SEARCH END_START END_STRING END_SUBTRACT
			END_UNSTRING END_WRITE
                        error
			END_IF

%left  THRU
%left  OR
%left  AND
%right  NOT
%left '<' '>' '=' NE LE GE
%left '-' '+'
%left '*' '/'
%right POW
%precedence NEG



%{
  static cbl_field_type_t
  set_operand_type(const cbl_refer_t& refer) {
    if( refer.field == NULL ) return FldInvalid;
    return refer.addr_of? FldPointer : refer.field->type;
  }

  static bool
  refer_pointer( const cbl_num_result_t& elem ) {
    assert(elem.refer.field);
    return elem.refer.is_pointer();
  }
  static bool
  valid_set_targets( const tgt_list_t& tgts, bool want_pointers ) {
    bool ok = true;
    // The only targets that can have addr_of are BASED or in Linkage Section.
    auto baddie = std::find_if( tgts.targets.begin(),
                                tgts.targets.end(),
                          []( const auto& num_result ) {
                          if( num_result.refer.addr_of ) {
                            auto f = num_result.refer.field;
                            if( ! (f->has_attr(based_e) || f->has_attr(linkage_e)) ) {
                              return true;
                            }
                          }
                          return false;
                        } );
    if( baddie != tgts.targets.end() ) {
     auto loc = symbol_field_location(field_index(baddie->refer.field));
     error_msg(loc,"target %s must be BASED or in LINKAGE SECTION",
               baddie->refer.name() );
      return false;
    }

    for( const auto& num_result : tgts.targets ) {
      auto loc = symbol_field_location(field_index(num_result.refer.field));
      if( refer_pointer(num_result) ) {
        if( !want_pointers ) {
          ok = false;
          error_msg( loc, "%s is a pointer", num_result.refer.name() );
        }
      } else {
        if( want_pointers ) {
          ok = false;
          error_msg( loc, "%s is not a pointer", num_result.refer.name() );
        }
      }
    }
    return ok;
  }

  static void initialize_allocated( cbl_refer_t input );
  static void
  initialize_statement( std::list<cbl_num_result_t>& tgts,
                        bool with_filler,
                        data_category_t category,
                        const category_map_t& replacement = category_map_t());


  unsigned char cbl_alphabet_t::nul_string[2] = ""; // 2 NULs lets us use one
  unsigned char *nul_string() { return cbl_alphabet_t::nul_string; }

  static inline literal_t literal_of( char *s ) {
                        literal_t output;
                        return output.set( strlen(s), s, "" );
  }
  static inline char * string_of( const literal_t& lit ) {
      return strlen(lit.data) == lit.len? lit.data : NULL;
  }

  static inline char * string_of( const REAL_VALUE_TYPE &cce ) {
      char output[64];
      real_to_decimal( output, &cce, sizeof(output), 32, 0 );

      char decimal = symbol_decimal_point();
      std::replace(output, output + strlen(output), '.', decimal);
      return xstrdup(output);
  }

  static inline char * string_of( tree cce ) {
      return string_of (TREE_REAL_CST (cce));
  }

  cbl_field_t *
  new_literal( const literal_t& lit, enum cbl_field_attr_t attr );

  static YYLTYPE first_line_of( YYLTYPE loc );
%}

%locations
%token-table
%define parse.error verbose // custom
%expect 6
%require "3.5.1"  //    3.8.2 also works, but not 3.8.0
%%

top:            programs
                {
                  if( ! goodnight_gracie() ) {
                    YYABORT;
                  }
                  if( nparse_error > 0 ) YYABORT;
                }
        |       programs end_program
                {
                  if( nparse_error > 0 ) YYABORT;
                }
                ;
programs:       program
        |       programs end_program program
                ;
program:	id_div options_para env_div data_div
                {
                  if( ! data_division_ready() ) {
		    mode_syntax_only(procedure_div_e);
		  }
                  current_division = procedure_div_e;
                }
                procedure_div
                {
                  if( yydebug ) labels_dump();
                }
                ;

id_div:         cdf_words IDENTIFICATION_DIV '.' program_id
        |	cdf_words                        program_id
        |       cdf_words IDENTIFICATION_DIV '.' function_id
                ;

cdf_words:	%empty
	|	cobol_words
		;
cobol_words:	cobol_words1
	|	cobol_words cobol_words1
		;
cobol_words1:	COBOL_WORDS EQUATE NAME[keyword] WITH NAME[name] {
		  if( ! tokens.equate(@keyword, $keyword, $name) ) { YYERROR; }
		}
	|	COBOL_WORDS UNDEFINE NAME[keyword] {
		  if( ! tokens.undefine(@keyword, $keyword) ) { YYERROR; }
		}
	|	COBOL_WORDS SUBSTITUTE NAME[keyword] BY NAME[name] {
		  if( ! tokens.substitute(@keyword, $keyword, $name) ) { YYERROR; }
		}
	|	COBOL_WORDS RESERVE NAME[name] {
		  if( ! tokens.reserve(@name, $name) ) { YYERROR; }
		}
		;

program_id:     PROGRAM_ID dot namestr[name] program_as program_attrs[attr] dot
                {
                  internal_ebcdic_lock();
                  current_division = identification_div_e;
                  parser_division( identification_div_e, NULL, 0, NULL );
                  location_set(@1);
                  int main_error=0;
                  const char *name = string_of($name);
                  parser_enter_program( name, false, &main_error );
                  if( main_error ) {
                    error_msg(@name, "PROGRAM-ID 'main' is invalid with -main option");
                    YYERROR;
                  }

                  if( symbols_begin() == symbols_end() ) {
                    symbol_table_init();
                  }
                  if( !current.new_program(@name, LblProgram, name,
		                           $program_as.data,
                                           $attr.common, $attr.initial) ) {
                    auto L = symbol_program(current_program_index(), name);
                    assert(L);
                    error_msg(@name, "PROGRAM-ID %s already defined on line %d",
                             name, L->line);
                    YYERROR;
                  }
                  if( nparse_error > 0 ) YYABORT;
                }
                ;
dot:            %empty
        |       '.'
                ;
program_as:     %empty     { static const literal_t empty {}; $$ = empty; }
        |       AS LITERAL { $$ = $2; }
                ;

function_id:    FUNCTION '.' NAME program_as program_attrs[attr] '.'
                {
                  internal_ebcdic_lock();
                  current_division = identification_div_e;
                  parser_division( identification_div_e, NULL, 0, NULL );
                  location_set(@1);

                  int main_error = 0;
                  parser_enter_program( $NAME, true, &main_error );
                  if( main_error ) {
                    error_msg(@NAME, "FUNCTION-ID 'main' is invalid with -main option");
                    YYERROR;
                  }
                  if( symbols_begin() == symbols_end() ) {
                    symbol_table_init();
                  }
                  if( !current.new_program(@NAME, LblFunction, $NAME,
		                      $program_as.data,
                                      $attr.common, $attr.initial) ) {
                    auto L = symbol_program(current_program_index(), $NAME);
                    assert(L);
                    error_msg(@NAME, "FUNCTION %s already defined on line %d",
                              $NAME, L->line);
                    YYERROR;
                  }
                  if( keyword_tok($NAME, true) ) {
		    error_msg(@NAME, "FUNCTION %s is an intrinsic function",
			      $NAME);
                    YYERROR;
                  }
                  current.udf_add(current_program_index());
                  if( nparse_error > 0 ) YYABORT;
                }
        |       FUNCTION '.' NAME program_as is PROTOTYPE '.'
                {
                  cbl_unimplemented("FUNCTION PROTOTYPE");
                }
                ;

options_para:   %empty
        |       OPTIONS opt_clauses '.'
        |       OPTIONS
                ;

opt_clauses:    opt_clause
        |       opt_clauses opt_clause
                ;
opt_clause:     opt_arith
        |       opt_round
        |       opt_entry
        |       opt_binary
        |       opt_decimal {
		  cbl_unimplementedw("type FLOAT-DECIMAL was ignored");
		}
        |       opt_intermediate
        |       opt_init
                ;

opt_arith:      ARITHMETIC is opt_arith_type {
		  if( ! current.option($opt_arith_type) ) {
		    error_msg(@3, "unable to set ARITHMETIC option");
		  }
		}
                ;
opt_arith_type: NATIVE		  { $$ = cbl_options_t::native_e; }
        |       STANDARD          { $$ = cbl_options_t::standard_e; }
        |       STANDARD_BINARY   { $$ = cbl_options_t::standard_binary_e; }
        |       STANDARD_DECIMAL  { $$ = cbl_options_t::standard_decimal_e; }
                ;
opt_round:      DEFAULT ROUNDED mode is rounded_type[type] {
                  current_rounded_mode($type);
                }
                ;
opt_entry:      ENTRY_CONVENTION is COBOL {
		  yywarn("ENTRY-CONVENTION IS COBOL, check");
		}
                ;
opt_binary:     FLOAT_BINARY default_kw is HIGH_ORDER_LEFT
		{
		  cbl_unimplementedw("HIGH-ORDER-LEFT was ignored");
		  if( ! current.option_binary(cbl_options_t::high_order_left_e) ) {
		    error_msg(@3, "unable to set HIGH_ORDER_LEFT");
		  }
		}
        |       FLOAT_BINARY default_kw is HIGH_ORDER_RIGHT[opt]
		{
		  cbl_unimplementedw("HIGH-ORDER-RIGHT was ignored");
		  if( ! current.option_binary(cbl_options_t::high_order_right_e) ) {
		    error_msg(@opt, "unable to set HIGH-ORDER-RIGHT");
		  }
		}
                ;
default_kw:     %empty
        |       DEFAULT
                ;
opt_decimal:    FLOAT_DECIMAL default_kw is HIGH_ORDER_LEFT[opt]
		{
		  cbl_unimplementedw("HIGH-ORDER-LEFT was ignored");
		  if( ! current.option_decimal(cbl_options_t::high_order_left_e) ) {
		    error_msg(@opt, "unable to set HIGH-ORDER-LEFT");
		  }
		}
        |       FLOAT_DECIMAL default_kw is HIGH_ORDER_RIGHT[opt]
		{
		  cbl_unimplementedw("HIGH-ORDER-RIGHT was ignored");
		  if( ! current.option_decimal(cbl_options_t::high_order_right_e) ) {
		    error_msg(@opt, "unable to set HIGH-ORDER-RIGHT");
		  }
		}
        |       FLOAT_DECIMAL default_kw is BINARY_ENCODING[opt]
		{
		  cbl_unimplementedw("BINARY-ENCODING was ignored");
		  if( ! current.option(cbl_options_t::binary_encoding_e) ) {
		    error_msg(@opt, "unable to set BINARY-ENCODING option");
		  }
		}
        |       FLOAT_DECIMAL default_kw is DECIMAL_ENCODING[opt]
		{
		  cbl_unimplementedw("DECIMAL-ENCODING was ignored");
		  if( ! current.option(cbl_options_t::decimal_encoding_e) ) {
		    error_msg(@opt, "unable to set DECIMAL-ENCODING option");
		  }
		}
                ;
opt_intermediate:
                INTERMEDIATE ROUNDING is round_between[round] {
		  current.intermediate_round($round);
		}
                ;

opt_init:       INITIALIZE opt_init_sects[sect] opt_section to opt_init_value[init]
		{
		  if( $sect.local ) {
		    current.initial_value(local_sect_e, $init);
		  }
		  if( $sect.working ) {
		    current.initial_value(working_sect_e, $init);
		  }
		}
                ;
opt_section:	%empty
	|	SECTION
		;
opt_init_sects: ALL { $$.local = $$.working = true; }
        |       opt_init_sect {
		  $$.local = $$.working = false;
		  switch($1) {
		    case local_sect_e:
		      $$.local = true; break;
		    case working_sect_e:
		      $$.working = true; break;
		    default: gcc_unreachable();
		  }
		}
        |       opt_init_sects opt_init_sect {
		  $$ = $1;
		  switch($2) {
		    case local_sect_e:
		      if( $$.local ) {
			error_msg(@2, "LOCAL-STORAGE repeated");
		      }
		      $$.local = true; break;
		    case working_sect_e:
		      if( $$.working ) {
			error_msg(@2, "WORKING-STORAGE repeated");
		      }
		      $$.working = true; break;
		    default: gcc_unreachable();
		  }
		}
                ;
opt_init_sect:  LOCAL_STORAGE   { $$ = local_sect_e; }
        |       SCREEN { cbl_unimplemented("SCREEN SECTION"); }
        |       WORKING_STORAGE { $$ = working_sect_e; }
        ;
opt_init_value: BINARY ZERO { $$ = constant_index(NULLS); }
	|       HIGH_VALUES { $$ = constant_index(HIGH_VALUES); }
	|       LITERAL
		{
		  if( $1.prefix[0] != 'X' ) {
		    error_msg(@1, "hexadecimal literal required");
		  }
		  if( $1.len != 1 ) {
		    error_msg(@1, "1-byte hexadecimal literal required");
		  }
      char ach[16];
      sprintf(ach, "%d", (int)($1.data[0]));
		  //auto f = new_literal($1.data);
		  auto f = new_literal(ach);
		  f = field_add(@1, f);
		  $$ = field_index(f);
		}
	|       LOW_VALUES  { $$ = constant_index(LOW_VALUES); }
	|       SPACES      { $$ = constant_index(SPACES); }
                ;

namestr:        ctx_name {
                  $$ = literal_of($1);
                  if( ! string_of($$) ) {
                    error_msg(@1, "'%s' has embedded NUL", $$.data);
                    YYERROR;
                  }
                }
        |       LITERAL {
                  if( $$.prefix[0] != '\0' ) {
                    error_msg(@1, "literal cannot use %s prefix in this context",
                              $$.prefix);
                    YYERROR;
                  }
                  if( !is_cobol_word($$.data) ) {
		    error_msg(@1, "literal '%s' must be a COBOL or C identifier",
                              $$.data);
                  }
                }
                ;

program_attrs:  %empty  { $$.common = $$.initial = $$.recursive = false; }
        |       is comminits program_kw { $$ = $2; }
                ;
comminits:      comminit
        |       comminits comminit {
                  if( ($1.initial && $2.recursive) ||
                      ($2.initial && $1.recursive) ) {
		    auto loc = $1.initial? @1 : @2;
                    error_msg(loc, "INITIAL cannot be used with RECURSIVE");
                  }
                  $$ = $1;
                  if( $2.common ) {
                    if( $1.common ) {
                      error_msg(@2, "COMMON repeated");
                    }
                    $$.common = $2.common;
                  }
                  if( $2.initial ) {
                    if( $1.initial ) {
                      error_msg(@2, "INITIAL repeated");
                    }
                    $$.initial = $2.initial;
                  }
                  if( $2.recursive ) {
                    if( $1.recursive ) {
                      error_msg(@2, "RECURSIVE repeated");
                    }
                    $$.recursive = $2.recursive;
                  }
                }
                ;
comminit:       COMMON     {
                  if( program_level() == 0 ) { // PROGRAM-ID being parsed not added yet.
                    error_msg(@1, "COMMON may be used only in a contained program");
                  }
                  $$.common = true;
                  $$.initial = $$.recursive = false;
                }
        |       INITIAL_kw { $$.initial = true;  $$.common = $$.recursive = false;}
        |       RECURSIVE  {
                  $$.recursive = true;  $$.common = $$.initial = false;
                }
                ;


env_div:        %empty              { current_division = environment_div_e; }
        |       ENVIRONMENT_DIV '.' { current_division = environment_div_e; }
        |       ENVIRONMENT_DIV '.' {
                  current_division = environment_div_e;
                } env_sections
                ;

env_sections:   env_section
        |       env_sections env_section
                ;

env_section:    INPUT_OUTPUT_SECT '.'
        |       INPUT_OUTPUT_SECT '.' io_sections
        |       INPUT_OUTPUT_SECT '.' selects { /* IBM requires FILE CONTROL.  */ }
        |       CONFIGURATION_SECT '.'
        |       CONFIGURATION_SECT '.' config_paragraphs
        |       cdf
                ;

io_sections:    io_section
        |       io_sections io_section
                ;

io_section:     FILE_CONTROL '.'
        |       FILE_CONTROL '.' selects
        |       IO_CONTROL '.'
        |       IO_CONTROL '.' io_control_clauses '.'
                ;

io_control_clauses: io_control_clause
        |           io_control_clauses io_control_clause
                ;
io_control_clause:
                SAME record area for_kw filenames
                {
                  symbol_file_same_record_area( $filenames->files );
                }
        |       SAME smerge area for_kw filenames
                {
                  symbol_file_same_record_area( $filenames->files );
                }
        |       APPLY COMMIT on field_list
                {
                  cbl_unimplementedw("I-O-CONTROL APPLY COMMIT");
                }
                ;
area:           %empty
        |       AREA
                ;
smerge:         SORT
        |       SORT_MERGE
                ;

selects:        select
        |       selects select
                ;

select:         SELECT optional NAME[name] select_clauses[clauses] '.'
                {
                  assert($clauses.file);
                  cbl_file_t *file = $clauses.file;

                  file->optional = $optional;
                  file->line = yylineno;
                  if( !namcpy(@clauses, file->name, $name) ) YYERROR;

                  if( ! ($clauses.clauses & assign_clause_e) ) {
                    error_msg(@name, "ASSIGN clause missing for %s", file->name);
                  }

                  // key check
                  switch(file->org) {
                  case file_indexed_e:
                    // indexed file cannot have relative key
                    if( ($clauses.clauses & relative_key_clause_e) != 0) {
                      assert(file->keys);
                      auto ikey = file->nkey - 1;
                      assert(file->keys[ikey].fields);
                      auto f = cbl_field_of(symbol_at(file->keys[ikey].fields[0]));
                      error_msg(@name, "INDEXED file %s cannot have RELATIVE key %s",
				file->name, f->name);
                      break; // because next message would be redundant
                    }
                    if( ($clauses.clauses & record_key_clause_e) == 0 ) {
                      error_msg(@name, "INDEXED file %s has no RECORD KEY",
				file->name);
                    }
                    break;
                  case file_disorganized_e:
                    file->org = file_sequential_e;
                    __attribute__((fallthrough));
                  default:
                    if( ($clauses.clauses & record_key_clause_e) != 0 ) {
                      assert(file->keys);
                      auto ikey = file->nkey - 1;
                      assert(file->keys[ikey].fields);
                      auto f = cbl_field_of(symbol_at(file->keys[ikey].fields[0]));
                      error_msg(@name, "%s file %s cannot have RECORD key %s",
				file_org_str(file->org), file->name, f->name);
                    }
                    break;
                  }

                  // access check
                  switch(file->access) {
                  case file_access_rnd_e:
                  case file_access_dyn_e:
                      if( is_sequential(file) ) {
                          error_msg(@name, "%s file %s cannot have ACCESS %s",
				    file_org_str(file->org), file->name,
				    file_access_str(file->access));
                      }
                      break;
                  default:
                    break;
                  }

                  // install file, and set record area's name
                  if( (file = file_add(@name, file)) == NULL ) YYERROR;
                  auto ifile = symbol_index(symbol_elem_of(file));
                  // update keys
                  for( auto p = file->keys;
                       p && p < file->keys + file->nkey; p++ )
                  {
                    if( p->name[0] == '\0' ) continue;
                    auto f = symbol_field(PROGRAM, 0, p->name);
                    cbl_field_of(f)->parent = ifile;
                    size_t isym = field_index(cbl_field_of(f));
                    update_symbol_map(symbol_at(isym));
                  }
                }
        |       SELECT optional NAME[name] '.'
                {
                  cbl_file_t file = protofile;

                  file.optional = $optional;
                  file.line = yylineno;
                  if( !namcpy(@name, file.name, $name) ) YYERROR;

                  if( file_add(@name, &file) == NULL ) YYERROR;
                }
                ;
selected_name:  external scalar { $$ = $2; }
        |       external LITERAL[name]
                {
                  const char *name = string_of($name);
                  if( ! name ) {
                    error_msg(@name, "'%s' has embedded NUL", $name.data);
                    YYERROR;
                  }
                  uint32_t len = $name.len;
                  cbl_field_t field {
                               0, FldLiteralA, FldInvalid, quoted_e | constant_e,
                               0, 0, 0, nonarray, 0, "", 0, cbl_field_t::linkage_t(),
				 {len,len,0,0, $name.data}, NULL };
                  field.attr |= literal_attr($name.prefix);
                  $$ = new cbl_refer_t( field_add(@name, &field) );
                }
                ;
external:       %empty /* GnuCOBOL uses EXTERNAL to control name resolution.  */
        |       EXTERNAL
                ;

select_clauses: select_clause { $$.clauses = $1.clause; $$.file = $1.file; }
        |       select_clauses[total] select_clause[part]
                {
                  $$ = $total;
                  // The default organization is sequential.
                  if( ($$.clauses & organization_clause_e) == 0 ) {
                    $$.file->org = file_sequential_e;
                  }
                  const bool exists = ($$.clauses & $part.clause);
                  $$.clauses |= $part.clause;

                  switch($part.clause) {
                  case alt_key_clause_e:
                    assert( $part.file->nkey == 1 );
                    if( $$.file->nkey++ == 0 ) {
                      // If no key yet exists, create room for it and the
                      // present alternate.
                      assert($$.file->keys == &cbl_file_t::no_key);
                      $$.file->keys = new cbl_file_key_t[++$$.file->nkey];
                    }
                    {
                      auto keys = new cbl_file_key_t[$$.file->nkey];
                      auto alt = std::copy($$.file->keys,
                                           $$.file->keys +
                                           $$.file->nkey - 1,
                                           keys);
                      // Assign the alternate key to the last element,
                      // and update the pointer.
                      *alt = $part.file->keys[0];
                      delete[] $$.file->keys;
                      $$.file->keys = keys;
                    }
                    break;
                  case assign_clause_e:
                    if( exists ) {
                      error_msg(@part, "clause is repeated");
                      YYERROR;
                    }
                    $$.file->filename = $part.file->filename;
                    break;
                  case collating_clause_e:
                    if( exists ) {
                      error_msg(@part, "clause is repeated");
                      YYERROR;
                    }
                    break;
                  case lock_mode_clause_e:
                    if( exists ) {
                      error_msg(@part, "clause is repeated");
                      YYERROR;
                    }
                    $$.file->lock = $part.file->lock;
                    break;
                  case organization_clause_e:
                    if( exists ) {
                      error_msg(@part, "clause is repeated");
                      YYERROR;
                    }
                    $$.file->org = $part.file->org;
                    break;
                  case padding_clause_e:
                  case reserve_clause_e:
                  case sharing_clause_e:
                  case record_delim_clause_e:
                    if( exists ) {
                      error_msg(@part, "clause is repeated");
                      YYERROR;
                    }
                    break;
                  case access_clause_e:
                    if( exists ) {
                      error_msg(@part, "clause is repeated");
                      YYERROR;
                    }
                    $$.file->access = $part.file->access;
                    break;
                  case relative_key_clause_e:
                    if( exists ) {
                      error_msg(@part, "clause is repeated");
                      YYERROR;
                    }
                    if( $$.clauses & record_key_clause_e ) {
                      error_msg(@part, "FILE %s is INDEXED, has no RELATIVE key",
                               $$.file->name);
                      YYERROR;
                    }
                    // fall thru
                  case record_key_clause_e:
                    if( exists ) {
                      error_msg(@part, "clause is repeated");
                      YYERROR;
                    }
                    if( ($$.clauses & relative_key_clause_e) &&
                         $part.clause == record_key_clause_e ) {
                      error_msg(@part, "FILE %s is RELATIVE, has no RECORD key",
                               $$.file->name);
                      YYERROR;
                    }
                    if( $$.file->nkey == 0 ) {
                      $$.file->nkey = $part.file->nkey;
                      $$.file->keys = $part.file->keys;
                    } else {
                      $$.file->keys[0] = $part.file->keys[0];
                    }
                    break;
                  /* case password_clause_e: */
                  case file_status_clause_e:
                    if( exists ) {
                      error_msg(@part, "clause is repeated");
                      YYERROR;
                    }
                    $$.file->user_status = $part.file->user_status;
                    $$.file->vsam_status = $part.file->vsam_status;
                    break;
                  }
                  if( $$.file->lock.locked() ) {
                    if( $$.file->org == file_sequential_e &&
                        $$.file->lock.multiple ) {
                      error_msg(@part, "SEQUENTIAL file cannot lock MULTIPLE records");
                    }
                  }

                  delete $part.file;
                }
                ;

select_clause:  access_clause
        |       alt_key_clause[alts]
        |       assign_clause[alts]
        |       collate_clause
        |       /* file */ status_clause
        |       lock_mode_clause
        |       org_clause
        |       padding_clause
        |       record_delim_clause
        |       record_key_clause
        |       relative_key_clause
        |       reserve_clause
        |       sharing_clause
                ;

access_clause:  ACCESS mode is access_mode[acc]
                {
                  $$.clause = access_clause_e;
                  $$.file = new cbl_file_t(protofile);
                  $$.file->access = static_cast<cbl_file_access_t>($acc);
                }
                ;
access_mode:    RANDOM      { $$ = file_access_rnd_e; }
        |       DYNAMIC     { $$ = file_access_dyn_e; }
        |       SEQUENTIAL  { $$ = file_access_seq_e; }
                ;

alt_key_clause: ALTERNATE record key is name key_source[fields] unique_key
                {
                  $$.clause = alt_key_clause_e;
                  $$.file = new cbl_file_t(protofile);
                  $$.file->nkey = 1;
                  if( $fields == NULL ) {
                    $$.file->keys = new cbl_file_key_t(field_index($name),
                                                       $unique_key);
                  } else {
                    $name->type  = FldLiteralA;
                    $name->data.initial = $name->name;
                    $name->attr |= record_key_e;
                    auto& name = *$name;
                    $$.file->keys = new cbl_file_key_t(name.name,
                                                       $fields->fields,
                                                       $unique_key);
                  }
                }
                ;
key_source:     %empty { $$ = NULL; }
        |       SOURCE is key_sources[fields] { $$ = $fields; }
        ;
key_sources:    name { $$ = new field_list_t($1); }
        |       key_sources name { $$ = $1; $$->fields.push_back($2); }
                ;
unique_key:     %empty          { $$ = true; }
        |       with DUPLICATES { $$ = false; }
                ;

assign_clause:  ASSIGN to selected_name[selected]  {
                  $$.clause = assign_clause_e;
                  $$.file = new cbl_file_t(protofile);
                  $$.file->filename = field_index($selected->field);
                }
        |       ASSIGN to device_name USING name {
                  $$.clause = assign_clause_e;
		  cbl_unimplemented("ASSIGN TO DEVICE");
		  YYERROR;
                }
        |       ASSIGN to device_name {
                  $$.clause = assign_clause_e;
		  cbl_unimplemented("ASSIGN TO DEVICE");
		  YYERROR;
                }
        |       ASSIGN USING name {
                  $$.clause = assign_clause_e;
                  $$.file = new cbl_file_t(protofile);
                  $$.file->filename = field_index($name);
                }
                ;

collate_clause: collate_claus1 {
                  $$.clause = collating_clause_e;
                  $$.file = new cbl_file_t(protofile);
                }
                ;
collate_claus1: collating SEQUENCE NAME /* SEQUENCE swallows IS/FOR */
        |       collating SEQUENCE ALPHANUMERIC is NAME
        |       collating SEQUENCE NATIONAL is NAME
        ;

status_clause:  file STATUS is name[user]
                {
                  $$.clause = file_status_clause_e;
                  $$.file = new cbl_file_t(protofile);
                  $$.file->user_status = field_index($user);
                }
        |       file STATUS is name[user] name[vsam]
                {
                  $$.clause = file_status_clause_e;
                  $$.file = new cbl_file_t(protofile);
                  $$.file->user_status = field_index($user);
                  $$.file->vsam_status = field_index($vsam);
                }
                ;

lock_mode_clause: // ISO only
                LOCK mode is lock_mode lock_how[how]
                {
                  $$.clause = lock_mode_clause_e;
                  $$.file = new cbl_file_t(protofile);
                  $$.file->lock.multiple = $how > 0;
                  if( ! $$.file->lock.mode_set($lock_mode) ) {
		    error_msg(@lock_mode, "logic error: %s is not a file lock mode",
		              keyword_str($lock_mode) );
		  }
                }
lock_how:       %empty { $$ = 0; }
        |       with LOCK_ON multiple records { $$ = $multiple; }
                ;
lock_mode:      MANUAL    { $$ = MANUAL; }
        |       RECORD    { $$ = RECORD; }
        |       AUTOMATIC { $$ = AUTOMATIC; }
                ;
multiple:       %empty    { $$ = 0; }
        |       MULTIPLE  { $$ = MULTIPLE; }
                ;
records:        RECORD
        |       RECORDS
                ;

org_clause:     org_clause1[org]
                {
                  $$.clause = organization_clause_e;
                  $$.file = new cbl_file_t(protofile);
                  $$.file->org = static_cast<cbl_file_org_t>($org);
                }
                ;
org_is:         %empty
        |       ORGANIZATION is
                ;
                // file_sequential is the proper default
org_clause1:    org_is      SEQUENTIAL { $$ = file_sequential_e; }
        |       org_is LINE SEQUENTIAL { $$ = file_line_sequential_e; }
        |       org_is      RELATIVE   { $$ = file_relative_e; }
        |       org_is      INDEXED    { $$ = file_indexed_e; }
                ;

                /*
                 * "The PADDING CHARACTER clause is syntax checked, but has no
                 * effect on the execution of the program."
                 */
padding_clause: PADDING character is padding_char
                {
                  $$.clause = padding_clause_e;
                  $$.file = new cbl_file_t(protofile);
                }
                ;
character:      %empty
        |       CHARACTER
                ;
padding_char:   NAME
        |       LITERAL
        |       NUMSTR
                ;

record_delim_clause:  RECORD DELIMITER is STANDARD_ALPHABET
                {
                  $$.clause = record_delim_clause_e;
                  $$.file = new cbl_file_t(protofile);
                }
                ;

record_key_clause: RECORD key is name key_source[fields]
                {
                  $$.clause = record_key_clause_e;
                  $$.file = new cbl_file_t(protofile);
                  $$.file->nkey = 1;
                  if( $fields == NULL ) {
                    $$.file->keys = new cbl_file_key_t(field_index($name));
                  } else { // "special" not-literal literal: a key name
                    $name->type = FldLiteralA;
                    $name->data.initial = $name->name;
                    $name->attr |= record_key_e;
                    $$.file->keys = new cbl_file_key_t($name->name,
                                                      $fields->fields, true);
                  }
                }
                ;

relative_key_clause: /* RELATIVE */ KEY is name
                { // lexer returns KEY for RELATIVE ... NAME
                  $$.clause = relative_key_clause_e;
                  $$.file = new cbl_file_t(protofile);
                  $$.file->nkey = 1;
                  $$.file->keys = new cbl_file_key_t(field_index($name));
                }
                ;

reserve_clause: RESERVE NUMSTR reserve_area
                {
                  $$.clause = reserve_clause_e;
                  $$.file = new cbl_file_t(protofile);
                }
                ;
reserve_area:   %empty
        |       AREA
        |       AREAS
                ;

sharing_clause: SHARING with sharing_who
                {
                  $$.clause = sharing_clause_e;
                  $$.file = new cbl_file_t(protofile);
                }
                ;
sharing_who:    ALL other
        |       NO other
        |       READ ONLY
                ;
other:          %empty
        |       OTHER
                ;

config_paragraphs: config_paragraph
        |       config_paragraphs config_paragraph
                ;

config_paragraph:
                SPECIAL_NAMES '.'
        |       SPECIAL_NAMES '.' specials '.'
        |       SOURCE_COMPUTER  '.' NAME with_debug '.'
        |       OBJECT_COMPUTER  '.' NAME collating_sequence[name] '.'
                {
                  if( $name ) {
                    if( !current.collating_sequence($name) ) {
                      error_msg(@name, "collating sequence already defined as '%s'",
                                current.collating_sequence());
                      YYERROR;
                    }
                  }
                }
        |       REPOSITORY '.'
        |       REPOSITORY '.' repo_members '.'
                ;

repo_members:   repo_member
        |       repo_members repo_member
                ;
repo_member:    repo_class
                { cbl_unimplemented("CLASS"); }
        |       repo_interface
                { cbl_unimplemented("INTERFACE"); }
        |       repo_func
        |       repo_program
        |       repo_property
                { cbl_unimplemented("PROPERTY"); }
                ;

repo_class:     CLASS NAME repo_as repo_expands
                ;
repo_as:        %empty      { $$ = literal_t(); }
        |       AS LITERAL  { $$ = $2; }
                ;
repo_expands:   %empty
        |       EXPANDS NAME USING NAME
                ;

repo_interface: INTERFACE NAME repo_as repo_expands
                ;

repo_func:      FUNCTION repo_func_names INTRINSIC
                {
		  auto namelocs( name_queue.pop() );
                  for( const auto& nameloc : namelocs ) {
                      current.repository_add(nameloc.name);
                  }
                }
        |       FUNCTION ALL INTRINSIC
                {
                  current.repository_add_all();
                }
        |       FUNCTION repo_func_names
                ;
repo_func_names:
                repo_func_name
        |       repo_func_names repo_func_name
                ;
repo_func_name: NAME {
                  if( ! current.repository_add($NAME) ) { // add intrinsic by name
                    auto token = current.udf_in($NAME);
                    if( !token ) {
                      error_msg(@NAME, "%s is not defined here as a user-defined function",
                                $NAME);
                      current.udf_dump();
                      YYERROR;
                    }
                    auto e = symbol_function(0, $NAME);
                    assert(e);
                    current.repository_add(symbol_index(e)); // add UDF to repository
                  }
                }
                ;

repo_program:   PROGRAM_kw NAME repo_as
                {
                  size_t parent = 0;
                  auto program = symbol_label( PROGRAM, LblProgram, 0, $NAME );
                  if( ! program ) {
                    if( $repo_as.empty() ) {
                      error_msg(@repo_as, "'%s' does not name an earlier program", $NAME);
                      YYERROR;
                    }
                    program = symbol_label( PROGRAM, LblProgram, 0,
                                            "", $repo_as.data );
                  }
                  if( ! program ) {
                    error_msg(@repo_as, "'%s' does not name an earlier program",
                             $repo_as.data);
                    YYERROR;
                  }
                  assert(program);
                  parent = symbol_index(symbol_elem_of(program));
                  // Literal field whose parent is the the aliased program.
                  cbl_field_t prog = {};
		  prog.type = FldLiteralA;
		  prog.attr = quoted_e;
		  prog.parent = parent;
		  prog.data.initial = $repo_as.data;
                  namcpy(@NAME, prog.name, $NAME);
                  if( ! prog.data.initial ) {
                    assert(program);
                    prog.data.initial = program->name;
                  }
                  auto e = symbol_field_add(PROGRAM, &prog);
                  symbol_field_location(symbol_index(e), @NAME);
                }
                ;

repo_property:  PROPERTY NAME repo_as
                ;

with_debug:     %empty
        |       with DEBUGGING MODE {
                  if( ! set_debug(true) ) {
                    error_msg(@2, "DEBUGGING MODE valid only in fixed format");
                  }
                }
                ;

collating_sequence: %empty { $$ = NULL; }
        |       PROGRAM_kw COLLATING SEQUENCE is NAME[name] { $$ = $name; }
        |       PROGRAM_kw           SEQUENCE is NAME[name] { $$ = $name; }
        |                  COLLATING SEQUENCE is NAME[name] { $$ = $name; }
        |                            SEQUENCE is NAME[name] { $$ = $name; }
                ;

specials:       special_names
                ;
special_names:  special_name
        |       special_names special_name
                ;

special_name:   dev_mnemonic
        |       ALPHABET NAME[name] is alphabet_name[abc]
                {
                  if( !$abc ) YYERROR;
                  assert($abc); // already in symbol table
                  if( !namcpy(@name, $abc->name, $name) ) YYERROR;
                  if( yydebug ) $abc->dump();
                }
        |       CLASS NAME is domains
                {
                  struct cbl_field_t field = { 0,
                    FldClass, FldInvalid, 0, 0, 0, 0, nonarray, yylineno, "",
                    0, cbl_field_t::linkage_t(),
		    {}, NULL };
                  if( !namcpy(@NAME, field.name, $2) ) YYERROR;

                  struct cbl_domain_t *domain =
                    new cbl_domain_t[ domains.size() + 1 ] ;

                  std::copy(domains.begin(), domains.end(), domain);

                  field.data.false_value_as($domains);
                  field.data.domain_as(domain);
                  domains.clear();

                  if( field_add(@2, &field) == NULL ) {
                    dbgmsg("failed class");
                    YYERROR;
                  }
                }
        |       CURRENCY sign is LITERAL[lit] with picture_sym
                {
                // The COBOL is "CURRENCY sign SYMBOL PICTURE symbol"
                // In our processing, we flip the order, and refer to
                // symbol_currency_add (symbol, sign-string). 'symbol' is the
                // character in the PICTURE string, and 'sign' is the substitution
                // that gets made in memory.
                  if( ! string_of($lit) ) {
                    error_msg(@lit, "'%s' has embedded NUL", $lit.data);
                    YYERROR;
                  }
                symbol_currency_add( $picture_sym, $lit.data );
                }
        |       DECIMAL_POINT is COMMA
                {
                  symbol_decimal_point_set(',');
                }
        |       LOCALE NAME is locale_spec
                {
                  current.locale($NAME, $locale_spec);
                  cbl_unimplemented("LOCALE syntax");
                }
                ;
        |       upsi
        |       SYMBOLIC characters symbolic is_alphabet
                {
                  cbl_unimplemented("SYMBOLIC syntax");
                }
                ;
locale_spec:    NAME    { $$ = $1; }
        |       LITERAL { $$ = string_of($1); }

                ;
symbolic:       NAME
        |       NUMSTR
        ;
is_alphabet:    ARE NUMSTR
        |       is NUMSTR
                ;

dev_mnemonic:	device_name is NAME
                {
                  cbl_special_name_t special = { $1.token, $1.id };
                  if( !namcpy(@NAME, special.name, $NAME) ) YYERROR;

                  const char *filename;

                  switch( special.id ) {
                  case STDIN_e: case SYSIN_e: case SYSIPT_e:
                    filename = "/dev/stdin";
                    break;
                  case STDOUT_e: case SYSOUT_e:
                  case SYSLIST_e: case SYSLST_e: case CONSOLE_e:
                    filename ="/dev/stdout";
                    break;
                  case STDERR_e: case SYSPUNCH_e: case SYSPCH_e: case SYSERR_e:
                    filename ="/dev/stderr";
                    break;
                  default:
                    filename ="/dev/null";
                    break;
                  }

                  special.filename = symbol_index(symbol_literalA(0, filename));

                  symbol_special_add(PROGRAM, &special);
                }
	|	NAME[device] is NAME[name]
		{
		  static const std::map< std::string, special_name_t > fujitsus
		  { // Fujitsu calls these "function names", not device names
		    { "ARGUMENT-NUMBER", ARG_NUM_e },
		    { "ARGUMENT-VALUE", ARG_VALUE_e } ,
		    { "ENVIRONMENT-NAME", ENV_NAME_e },
		    { "ENVIRONMENT-VALUE", ENV_VALUE_e },
		  };
		  std::string device($device);
		  std::transform($device, $device + strlen($device),
		                 device.begin(), toupper);
		  auto p = fujitsus.find(device.c_str());
		  if( p == fujitsus.end() ) {
		    error_msg(@device, "%s is not a device name");
		  }

                  cbl_special_name_t special = { 0, p->second };
                  if( !namcpy(@name, special.name, $name) ) YYERROR;

                  symbol_special_add(PROGRAM, &special);
		}
                ;

device_name:	SYSIN           { $$.token = SYSIN; $$.id = SYSIN_e; }
        |       SYSIPT          { $$.token = SYSIPT; $$.id = SYSIPT_e; }
        |       SYSOUT          { $$.token = SYSOUT; $$.id = SYSOUT_e; }
        |       SYSLIST         { $$.token = SYSLIST; $$.id = SYSLIST_e; }
        |       SYSLST          { $$.token = SYSLST; $$.id = SYSLST_e; }
        |       SYSPUNCH        { $$.token = SYSPUNCH; $$.id = SYSPUNCH_e; }
        |       SYSPCH          { $$.token = SYSPCH; $$.id = SYSPCH_e; }
        |       CONSOLE         { $$.token = CONSOLE; $$.id = CONSOLE_e; }
        |       C01             { $$.token = C01; $$.id = C01_e; }
        |       C02             { $$.token = C02; $$.id = C02_e; }
        |       C03             { $$.token = C03; $$.id = C03_e; }
        |       C04             { $$.token = C04; $$.id = C04_e; }
        |       C05             { $$.token = C05; $$.id = C05_e; }
        |       C06             { $$.token = C06; $$.id = C06_e; }
        |       C07             { $$.token = C07; $$.id = C07_e; }
        |       C08             { $$.token = C08; $$.id = C08_e; }
        |       C09             { $$.token = C09; $$.id = C09_e; }
        |       C10             { $$.token = C10; $$.id = C10_e; }
        |       C11             { $$.token = C11; $$.id = C11_e; }
        |       C12             { $$.token = C12; $$.id = C12_e; }
        |       CSP             { $$.token = CSP; $$.id = CSP_e; }
        |       S01             { $$.token = S01; $$.id = S01_e; }
        |       S02             { $$.token = S02; $$.id = S02_e; }
        |       S03             { $$.token = S03; $$.id = S03_e; }
        |       S04             { $$.token = S04; $$.id = S04_e; }
        |       S05             { $$.token = S05; $$.id = S05_e; }
        |       AFP_5A          { $$.token = AFP_5A; $$.id = AFP_5A_e; }
        |       STDIN           { $$.token = STDIN; $$.id = STDIN_e; }
        |       STDOUT          { $$.token = STDOUT; $$.id = STDOUT_e; }
        |       STDERR          { $$.token = STDERR; $$.id = STDERR_e; }
                ;

alphabet_name:  STANDARD_ALPHABET  { $$ = alphabet_add(@1, ASCII_e); }
        |       NATIVE             { $$ = alphabet_add(@1, EBCDIC_e); }
        |       EBCDIC             { $$ = alphabet_add(@1, EBCDIC_e); }
        |       alphabet_seqs
                {
                  $$ = cbl_alphabet_of(symbol_alphabet_add(PROGRAM, $1));
                }
        |       error
                {
                  error_msg(@1, "code-name-1 may be STANDARD-1, STANDARD-2, "
                            "NATIVE, OR EBCDIC");
                  $$ = NULL;
                }
                ;
alphabet_seqs:  alphabet_seq[seq]
                /*
                 * The 1st element of the 1st sequence represents the
                 * low-value; its index becomes cbl_alphabet_t::low_index.  The
                 * high_index belongs to the last element of the last sequence
                 * that is not an ALSO.
                 */
                {
                  $$ = new cbl_alphabet_t(@seq, custom_encoding_e);

                  if( !$seq.low || $seq.also ) {
                    error_msg(@1, "syntax error at ALSO");
                    YYERROR;
                  }
                  $$->add_sequence(@seq, $seq.low);
                  size_t len = $seq.low == nul_string()? 1 : strlen((const char*)$seq.low);
                  assert(len > 0);
                  $$->add_interval(@seq, $seq.low[--len], $seq.high[0]);
                  $$->add_sequence(@seq, $seq.high);
                }
        |       alphabet_seqs alphabet_seq[seq]
                {
                  // ALSO x'00' is valid, but in that case the low pointer is NULL
                  if( !$seq.low ) {
                    $$->also(@seq, $seq.also);
                  } else {
                    $$->add_sequence(@seq, $seq.low);
                    size_t len = $seq.low == nul_string()? 1 : strlen((const char*)$seq.low);
                    assert(len > 0);
                    $$->add_interval(@seq, $seq.low[--len], $seq.high[0]);
                    $$->add_sequence(@seq, $seq.high);
                  }
                }
                ;
alphabet_seq:   alphabet_lit[low]
                {
                  $$.also = 0;
                  if( $low.len == 1 && $low.data[0] == '\0' ) {
                    $$.high = $$.low = nul_string();
                  } else {
                    size_t size = 1 + $low.len;
                    $$.low = new unsigned char[size];
                    memcpy($$.low, $low.data, size);
                    $$.high = $$.low + size - 1;
                    assert($$.high[0] == '\0');
                  }
                }
        |       alphabet_lit[low] THRU alphabet_lit[high]
                {
                  $$.also = 0;
                  size_t size = 1 + $low.len;
                  if( $low.len == 1 && $low.data[0] == '\0' ) {
                    $$.low = nul_string();
                  } else {
                    $$.low = new unsigned char[size];
                    memcpy($$.low, $low.data, size);
                  }
                  assert($high.len > 0);
                  assert($high.data[0] != '\0');
                  size = 1 + $high.len;
                  $$.high = new unsigned char[size];
                  memcpy($$.high, $high.data, size);
                }
        |       ALSO alphabet_etc { $$ = {}; $$.also = $2; }
                ;
alphabet_etc:   alphabet_lit
                {
                  if( $1.len > 1 ) {
                    error_msg(@1, "'%c' can be only a single letter", $1.data);
                    YYERROR;
                  }
                  $$ = (unsigned char)$1.data[0];
                }
        |       spaces_etc {
                  // For figurative constants, pass the synmbol table index,
                  // marked with the high bit.
                  static const auto bits = sizeof($$) * 8 - 1;
                  $$ = 1;
                  $$ = $$ << bits;
                  $$ |= constant_index($1);
                }
                ;
alphabet_lit:   LITERAL { $$ = $1; assert($$.len > 0); }
        |       NUMSTR  {
                  assert( $1.radix == decimal_e);
                  $$ = literal_of($1.string);
                }
                ;

upsi:           UPSI is NAME
                {
                  assert($UPSI);
                  size_t parent = symbol_index(symbol_field(0,0,"UPSI-0"));
                  cbl_field_t *field = field_alloc(@NAME, FldSwitch, parent, $NAME);
                  if( !field ) YYERROR;
                  field->attr = constant_e;
                  field->data.initial = $UPSI;
                }
        |       UPSI is NAME upsi_entry[entry]
                {
                  assert($UPSI);
                  size_t parent = symbol_index(symbol_field(0,0,"UPSI-0"));
                  cbl_field_t *field = field_alloc(@NAME, FldSwitch, parent, $NAME);
                  if( !field ) YYERROR;
                  field->attr = constant_e;
                  field->data.initial = $UPSI;

                  assert('0' <= $UPSI[0] && $UPSI[0] < '8');
                  const uint32_t bitn = $UPSI[0] - '0', value = (1 << bitn);

                  if( $entry.on ) {
                    cbl_field_t *on = field_alloc(@NAME, FldSwitch, parent, $entry.on);
                    if( !on ) YYERROR;
                    on->data = new cbl_upsi_mask_t(true, value);
                  }
                  if( $entry.off ) {
                    cbl_field_t *off = field_alloc(@NAME, FldSwitch, parent, $entry.off);
                    if( !off ) YYERROR;
                    off->data = new cbl_upsi_mask_t(false, value);
                  }
                }
        |       UPSI upsi_entry[entry]
                {
                  size_t parent = symbol_index(symbol_field(0,0,"UPSI-0"));
                  assert('0' <= $UPSI[0] && $UPSI[0] < '8');
                  const uint32_t bitn = $UPSI[0] - '0', value = (1 << bitn);

                  if( $entry.on ) {
                    cbl_field_t *on = field_alloc($entry.loc, FldSwitch, parent, $entry.on);
                    if( !on ) YYERROR;
                    on->data = new cbl_upsi_mask_t(true, value);
                  }
                  if( $entry.off ) {
                    cbl_field_t *off = field_alloc($entry.loc, FldSwitch, parent, $entry.off);
                    if( !off ) YYERROR;
                    off->data = new cbl_upsi_mask_t(false, value);
                  }
                }
                ;
upsi_entry:     ON  status is NAME
                {
                  $$.loc = @NAME;
                  $$.on  = $NAME;
                  $$.off = NULL;
                }
        |       OFF status is NAME
                {
                  $$.loc = @NAME;
                  $$.on  = NULL;
                  $$.off = $NAME;
                }
        |       OFF status is NAME[off] ON  status is NAME[on]
                {
                  $$.loc = @off;
                  $$.on  = $on;
                  $$.off = $off;
                }
        |       ON  status is NAME[on] OFF status is NAME[off]
                {
                  $$.loc = @on;
                  $$.on =  $on;
                  $$.off = $off;
                }
                ;

picture_sym:    %empty                { $$ = NULL; }
        |       PICTURE SYMBOL LITERAL[lit] {
                  if( ! string_of($lit) ) {
                    error_msg(@lit, "'%s' has embedded NUL", $lit.data);
                    YYERROR;
                  }
                  $$ = string_of($lit);
                }
                ;

                /*
                 * The domains nonterminal ($domain) carries the FALSE value,
                 * if any.  The domains variable (global std::list) carries the
                 * variable's DOMAIN, ending in a NULL. See the action for
                 * "CLASS NAME is domains".
                 */
domains:        domain
        |       domains domain { $$ = $1? $1 : $2; }
                ;

domain:         all LITERAL[a]
                {
                  if( ! string_of($a) ) {
		    gcc_location_set(@a);
                    yywarn("'%s' has embedded NUL", $a.data);
                  }
                  $$ = NULL;
                  cbl_domain_t domain(@a, $all, $a.len, $a.data);
                  domains.push_back(domain);
                }
        |       all[a_all] LITERAL[a] THRU all[z_all] LITERAL[z]
                {
                  if( ! string_of($a) ) {
                    yywarn("'%s' has embedded NUL", $a.data);
                  }
                  if( ! string_of($z) ) {
                    yywarn("'%s' has embedded NUL", $z.data);
                  }
                  $$ = NULL;
                  cbl_domain_elem_t first(@a, $a_all, $a.len, $a.data),
                                     last(@z, $z_all, $z.len, $z.data);
                  domains.push_back(cbl_domain_t(first, last));
                }
        |       all NUMSTR[n]
                {
                  $$ = NULL;
                  cbl_domain_t dom(@n, $all, strlen($n.string), $n.string, true);
                  domains.push_back(dom);
                }
        |       all[n_all] NUMSTR[n] THRU all[m_all] NUMSTR[m]
                {
                  $$ = NULL;
                  cbl_domain_elem_t first(@n, $n_all, strlen($n.string), $n.string, true),
		                     last(@m, $m_all, strlen($m.string), $m.string, true);
                  domains.push_back(cbl_domain_t(first, last));
                }
        |       all reserved_value {
                  $$ = NULL;
                  if( $2 == NULLS ) YYERROR;
                  auto value = constant_of(constant_index($2))->data.initial;
                  struct cbl_domain_t domain( @2, $all, strlen(value), value );
                  domains.push_back(domain);
                }
        |       all[a_all] reserved_value[a] THRU all[z_all] LITERAL[z] {
                  if( ! string_of($z) ) {
                    yywarn("'%s' has embedded NUL", $z.data);
                  }
                  $$ = NULL;
                  if( $a == NULLS ) YYERROR;
                  auto value = constant_of(constant_index($a))->data.initial;
                  cbl_domain_elem_t first(@a, $a_all, strlen(value), value),
                                     last(@z, $z_all, $z.len, $z.data);
                  domains.push_back(cbl_domain_t(first, last));
                }
        |       all[a_all] reserved_value[a] THRU all[z_all] NUMSTR[z] {
                  $$ = NULL;
                  if( $a == NULLS ) YYERROR;
                  auto value = constant_of(constant_index($a))->data.initial;
                  cbl_domain_elem_t first(@a, $a_all, strlen(value), value, true),
                                     last(@z, $z_all, strlen($z.string), $z.string, true);
                  domains.push_back(cbl_domain_t(first, last));
                }
        |       when_set_to FALSE_kw is LITERAL[value]
                {
                  if( ! string_of($value) ) {
                    yywarn("'%s' has embedded NUL", $value.data);
                  }
                  char *dom = $value.data;
                  $$ = new cbl_domain_t(@value, false, $value.len, dom);
                }
        |       when_set_to FALSE_kw is reserved_value
                {
                  if( $4 == NULLS ) YYERROR;
                  auto value = constant_of(constant_index($4))->data.initial;
                  $$ = new cbl_domain_t(@4, false, strlen(value), value );
                }
        |       when_set_to FALSE_kw is NUMSTR[n]
                {
		  $$ = new cbl_domain_t(@n, false, strlen($n.string), $n.string, true);
                }
                ;
when_set_to:    %empty
        |       WHEN
        |            SET
        |                TO
        |       WHEN SET
        |            SET TO
        |       WHEN     TO
        |       WHEN SET TO
        ;

data_div:       %empty
        |       DATA_DIV
        |       DATA_DIV { current_division = data_div_e; } data_sections
                {
                  current_data_section = not_data_datasect_e;
                  parser_division( data_div_e, NULL, 0, NULL );
                }
                ;

data_sections:  data_section
        |       data_sections data_section
                ;

data_section:   FILE_SECT '.'
        |       FILE_SECT '.' {
                  current_data_section_set(@1, file_datasect_e);
                } file_descrs
        |       WORKING_STORAGE_SECT '.' {
                  current_data_section_set(@1, working_storage_datasect_e);
                } fields_maybe
        |       LOCAL_STORAGE_SECT '.' {
                  current_data_section_set(@1, local_storage_datasect_e);
                } fields_maybe
        |       LINKAGE_SECT '.' {
                  current_data_section_set(@1, linkage_datasect_e);
                } fields_maybe
	|	SCREEN SECTION '.' {
		  cbl_unimplemented("SCREEN SECTION");
		}
                ;

file_descrs:    file_descr
        |       file_descrs file_descr
                ;
file_descr:     fd_name            '.' { field_done(); } fields
        |       fd_name fd_clauses '.' { field_done(); } fields
                ;

fd_name:        FD NAME { $$ = $2; file_section_fd_set(fd_e, $2, @2); }
	|       SD NAME { $$ = $2; file_section_fd_set(sd_e, $2, @2); }
        ;

fd_clauses:     fd_clause
        |       fd_clauses fd_clause
                ;
fd_clause:      record_desc
                {
                  auto f = cbl_file_of(symbol_at(file_section_fd));
                  f->varying_size.min = $1.min;
                  f->varying_size.max = $1.max;
                  auto& cap = cbl_field_of(symbol_at(f->default_record))->data.capacity;
                  cap = std::max(cap, uint32_t(f->varying_size.max));
                  // If min != max now, we know varying is explicitly defined.
                  f->varying_size.explicitly = f->varies();
                  if( f->varying_size.max != 0 ) {
                    if( !(f->varying_size.min <= f->varying_size.max) ) {
                      error_msg(@1, "%zu must be <= %zu",
                                f->varying_size.min, f->varying_size.max);
                      YYERROR;
                    }
                  }
                }
        |       block_desc
        |       label_desc
        |       DATA record_is field_list
        |       RECORDING mode is NAME
                {
                  switch( $NAME[0] ) {
                  case 'F':
                  case 'V':
                  case 'U':
                  case 'S':
                    break;
                  default:
                    error_msg(@NAME, "invalid RECORDING MODE '%s'", $NAME);
                    YYERROR;
                  }
                  cbl_unimplementedw("RECORDING MODE was ignored, not defined by ISO 2023");
                }
        |       VALUE OF fd_values
        |       CODESET is NAME
        |       is GLOBAL
                {
                  auto f = cbl_file_of(symbol_at(file_section_fd));
                  f->attr |= global_e;
                }
        |       is EXTERNAL
                {
                  auto f = cbl_file_of(symbol_at(file_section_fd));
                  f->attr |= external_e;
                }
        |       is EXTERNAL as LITERAL
                {
                  auto f = cbl_file_of(symbol_at(file_section_fd));
                  f->attr |= external_e;
                  cbl_unimplemented("AS LITERAL ");
                }
        |       fd_linage
        |       fd_report {
                  cbl_unimplemented("REPORT WRITER");
                  YYERROR;
                }
                ;

block_desc:     BLOCK_kw contains rec_contains chars_recs
                ;
rec_contains:   NUMSTR[min] {
                  REAL_VALUE_TYPE rn = numstr2i($min.string, $min.radix);
                  ssize_t n = real_to_integer (&rn);
                  if( n < 0 ) {
                    error_msg(@min, "size %s cannot be negative", $min.string);
                    YYERROR;
                  }
                  $$.min = $$.max = n; // fixed length
                }
        |       NUMSTR[min] TO NUMSTR[max] {
                  REAL_VALUE_TYPE rn = numstr2i($min.string, $min.radix);
                  ssize_t n = real_to_integer (&rn);
                  if( n < 0 ) {
                    error_msg(@min, "size %s cannot be negative", $min.string);
                    YYERROR;
                  }
                  $$.min = n;

                  rn = numstr2i($max.string, $max.radix);
                  n = real_to_integer (&rn);
                  if( n < 0 ) {
                    error_msg(@max, "size %s cannot be negative", $max.string);
                    YYERROR;
                  }
                  $$.max = n;
                  if( !($$.min < $$.max) ) {
                    error_msg(@max, "FROM (%xz) must be less than TO (%zu)",
                              $$.min, $$.max);
                    YYERROR;
                  }
                }
                ;
chars_recs:     %empty
        |       CHARACTERS
        |       RECORDS
                ;

label_desc:     LABEL record_is STANDARD
        |       LABEL record_is OMITTED
        |       LABEL record_is fd_labels
                ;

record_is:      RECORD /* lexer swallows IS/ARE */
        |       RECORDS
                ;

fd_values:      fd_value
        |       fd_values fd_value
                ;
                /* "The VALUE OF clause is syntax checked, but has
                    no effect on the execution of the program." */
fd_value:       NAME is alpha_val
                ;
alpha_val:      alphaval
        |       scalar
                ;

fd_labels:      fd_label
        |       fd_labels fd_label
                ;
fd_label:       NAME
        ;

record_desc:    RECORD is       record_vary[r] depending   { $$ = $r; }
        |       RECORD contains rec_contains[r] characters { $$ = $r; }
                ;

record_vary:    VARYING in_size from_to { $$ = $from_to; }
        |       VARYING         from_to { $$ = $from_to; }
        |       VARYING in_size { $$.min = 0; $$.max = 0; }
        |       VARYING         { $$.min = 0; $$.max = 0; }
        ;

in_size:        IN SIZE
        |       IN
        |          SIZE
        ;

from_to:        FROM NUMSTR[min] TO NUMSTR[max] characters {
                  REAL_VALUE_TYPE rn = numstr2i($min.string, $min.radix);
                  ssize_t n = real_to_integer (&rn);
                  if( n < 0 ) {
                    error_msg(@min, "size %s cannot be negative", $min.string);
                    YYERROR;
                  }
                  $$.min = n;
                  rn = numstr2i($max.string, $max.radix);
                  n = real_to_integer (&rn);
                  if( n < 0 ) {
                    error_msg(@min, "size %s cannot be negative", $max.string);
                    YYERROR;
                  }
                  $$.max = n;
                }
        |       NUMSTR[min] TO NUMSTR[max] characters {
                  REAL_VALUE_TYPE rn = numstr2i($min.string, $min.radix);
                  ssize_t n = real_to_integer (&rn);
                  if( n < 0 ) {
                    error_msg(@min, "size %s cannot be negative", $min.string);
                    YYERROR;
                  }
                  $$.min = n;
                  rn = numstr2i($max.string, $max.radix);
                  n = real_to_integer (&rn);
                  if( n < 0 ) {
                    error_msg(@max, "size %s cannot be negative", $max.string);
                    YYERROR;
                  }
                  $$.max = n;
                }

        |       TO NUMSTR[max] characters {
                  REAL_VALUE_TYPE rn = numstr2i($max.string, $max.radix);
                  ssize_t n = real_to_integer (&rn);
                  if( n < 0 ) {
                    error_msg(@max, "size %s cannot be negative", $max.string);
                    YYERROR;
                  }
                  $$.min = 0;
                  $$.max = n;
                }

        |       FROM NUMSTR[min] characters {
                  REAL_VALUE_TYPE rn = numstr2i($min.string, $min.radix);
                  ssize_t n = real_to_integer (&rn);
                  if( n < 0 ) {
                    error_msg(@min, "size %s cannot be negative", $min.string);
                    YYERROR;
                  }
                  $$.min = n;
                  $$.max = size_t(-1);
                }
        |       NUMSTR[min] characters {
                  REAL_VALUE_TYPE rn = numstr2i($min.string, $min.radix);
                  ssize_t n = real_to_integer (&rn);
                  if( n < 0 ) {
                    error_msg(@min, "size %s cannot be negative", $min.string);
                    YYERROR;
                  }
                  $$.min = n;
                  $$.max = size_t(-1);
                }

        |       CHARACTERS { $$.min = 0; $$.max = size_t(-1); }
        ;

depending:      %empty
        |       DEPENDING on NAME
                {
                  assert(file_section_fd > 0);
                  symbol_elem_t *e = symbol_at(file_section_fd);
                  assert(e);
                  auto file = cbl_file_of(e);
                  size_t odo;

                  if( (e = symbol_field(PROGRAM, 0, $3)) != NULL ) {
                    assert(e->type == SymField);
                    odo = symbol_index(e);
                  } else {
		    e = symbol_field_forward_add(PROGRAM, 0, $NAME, yylineno);
                    if( !e ) YYERROR;
		    symbol_field_location( symbol_index(e), @NAME );
                    odo = field_index(cbl_field_of(e));
                  }

                  file->record_length = odo;
                  assert( file->record_length > 0 );
                }
                ;

fd_linage:      LINAGE is num_value with_footings
        |       LINAGE is num_value lines
        ;
with_footings:  with_footing
        |       with_footings with_footing
                ;
with_footing:   lines with FOOTING at num_value
        |       lines at top_bot num_value
                ;
top_bot:        TOP
        |       BOTTOM
                ;

fd_report:      REPORT
        |       REPORTS
                ;

fields_maybe:   %empty
        |       fields
                ;
fields:         field
        |       fields field
                ;

field:          cdf
        |       data_descr '.'
                {
                  if( in_file_section() && $data_descr->level == 1 ) {
                    if( !file_section_parent_set($data_descr) ) {
                      YYERROR;
                    }
                  }
                  field_done();

                  const auto& field(*$data_descr);

                  // Format data.initial per picture
                  if( 0 == pristine_values.count(field.data.initial) ) {
                    if( field.data.digits > 0 && !field.is_zero() ) {
                      char *initial;
                      int rdigits = field.data.rdigits < 0?
                                    1 : field.data.rdigits + 1;

                      if( field.has_attr(scaled_e) ) {
                        if( field.data.rdigits > 0 ) {
                          rdigits = field.data.digits + field.data.rdigits;
                        } else {
                          rdigits = 0;
                        }
                      }
                      initial = string_of(field.data.value_of());
                      if( !initial ) {
                        error_msg(@1, xstrerror(errno));
                        YYERROR;
                      }
                      char decimal = symbol_decimal_point();
                      std::replace(initial, initial + strlen(initial), '.', decimal);
                      free(const_cast<char*>($data_descr->data.initial));
                      $data_descr->data.initial = initial;
                      if( yydebug ) {
                        const char *value_str = string_of(field.data.value_of());
                        dbgmsg("%s::data.initial is (%%%d.%d) %s ==> '%s'",
			       field.name,
			       field.data.digits,
			       rdigits,
			       value_str? value_str : "",
			       field.data.initial);
                      }
                    }
                  }
                }
                ;

occurs_clause:  OCCURS cardinal_lb             indexed
        |       OCCURS cardinal_lb  key_descrs indexed
        |       OCCURS depending_on key_descrs indexed
        |       OCCURS depending_on            indexed
        |       OCCURS name                    indexed
		{
		  if( ! (is_constant($name) && $name->type == FldLiteralN) ) {
		    error_msg(@name, "%s is not CONSTANT", $name->name);
		    YYERROR;
		  }
                  cbl_occurs_t *occurs = &current_field()->occurs;
                  occurs->bounds.lower =
                  occurs->bounds.upper = $name->as_integer();
		}
                ;
cardinal_lb:    cardinal times {
                  current_field()->occurs.bounds.lower = $cardinal;
                  current_field()->occurs.bounds.upper = $cardinal;
                }
                ;

cardinal:       NUMSTR[input]
                {
                  REAL_VALUE_TYPE rn = numstr2i($input.string, $input.radix);
                  $$ = real_to_integer (&rn);
                }
                ;

depending_on:   cardinal[lower] TO bound DEPENDING on name
                {
                  cbl_occurs_t *occurs = &current_field()->occurs;
                  occurs->bounds.lower = (size_t)$lower;
                  occurs->bounds.upper = (size_t)$bound;
                  occurs->depending_on = field_index($name);
                }
        |                          bound DEPENDING on name
                {
                  cbl_occurs_t *occurs = &current_field()->occurs;
                  occurs->bounds.lower = 1;
                  occurs->bounds.upper = (size_t)$bound;
                  occurs->depending_on = field_index($name);
                }
                ;
bound:          cardinal  times
        |       UNBOUNDED times  { $$ = -1; }
                ;

key_descrs:     key_descr
        |       key_descrs key_descr
                ;
key_descr:      ordering key is key_fields
                ;
ordering:       ASCENDING
                {
                  current_field()->occurs.key_alloc(true);
                }
        |       DESCENDING
                {
                  current_field()->occurs.key_alloc(false);
                }
                ;
key_fields:     key_field1
        |       key_fields key_field1
                ;
key_field1:     name
                {
                  current_field()->occurs.key_field_add($1);
                }
                ;

indexed:        %empty
        |       INDEXED by index_fields
                ;
index_fields:   index_field1
        |       index_fields index_field1
                ;
index_field1:   ctx_name[name]
                {
		  static const cbl_field_data_t data { 0, 8 }; // capacity 8
                  cbl_field_t field = {};
		  field.type = FldIndex;
		  field.parent = field_index(current_field());
		  field.data = data;
                  if( !namcpy(@name, field.name, $name) ) YYERROR;

                  auto symbol = symbol_field(PROGRAM, 0, $name);
                  if( symbol ) {
                    auto field( cbl_field_of(symbol) );
                    error_msg(@name,  "'%s' already defined on line %d",
                              field->name, field->line );
                    YYERROR;
                  }

                  auto index = field_add(@name, &field);
                  if( !index ) {
                    YYERROR;
                  }

                  current_field()->occurs.index_add(index);
                }
                ;

level_name:     LEVEL ctx_name
                {
                  switch($LEVEL) {
                  case 66:
                  case 77:
                  case 88:
                    break;
                  default:
		    if( 1 <= $LEVEL && $LEVEL <= 49 ) break;
                    error_msg(@LEVEL, "LEVEL %d not supported", $LEVEL);
                    YYERROR;
                  }
                  struct cbl_field_t field = { 0,
                    FldInvalid, FldInvalid, 0, 0, 0, capacity_cast($1),
		    nonarray, yylineno, "",
                    0, cbl_field_t::linkage_t(),
		    {}, NULL };
                  if( !namcpy(@ctx_name, field.name, $2) ) YYERROR;

                  $$ = field_add(@$, &field);
                  if( !$$ ) {
                    YYERROR;
                  }
                  current_field($$); // make available for data_clauses
                }
        |       LEVEL
                {
                  switch($LEVEL) {
                  case 66:
                  case 77:
                  case 88:
                    break;
                  default:
		    if( 1 <= $LEVEL && $LEVEL <= 49 ) break;
                    error_msg(@LEVEL, "LEVEL %d not supported", $LEVEL);
                    YYERROR;
                  }
                  struct cbl_field_t field = { 0,
                    FldInvalid, FldInvalid, 0, 0, 0, capacity_cast($1),
		    nonarray, yylineno, "",
                    0, {}, {}, NULL };

                  $$ = field_add(@1, &field);
                  if( !$$ ) {
                    YYERROR;
                  }
                  current_field($$); // make available for data_clauses
                }
                ;

data_descr:     data_descr1
                {
                  $$ = current_field($1); // make available for occurs, etc.
                }
        |       error { static cbl_field_t none = {}; $$ = &none; }
                ;

const_value:    cce_expr
        |       BYTE_LENGTH of name { $name->data.set_real_from_capacity(&$$); }
        |       LENGTH      of name { $name->data.set_real_from_capacity(&$$); }
        |       LENGTH_OF   of name { $name->data.set_real_from_capacity(&$$); }
                ;

value78:        literalism
                {
                  cbl_field_data_t data = {};
		    data.capacity = capacity_cast(strlen($1.data));
		    data.initial = $1.data;
                  $$ = new cbl_field_data_t(data);
                }
        |       const_value
                {
                  cbl_field_data_t data = {};
		  data = build_real (float128_type_node, $1);
                  $$ = new cbl_field_data_t(data);
                }
        |       true_false
                {
                  cbl_unimplemented("Boolean constant");
                  YYERROR;
                }
                ;

data_descr1:    level_name
                {
                  assert($1 == current_field());
                  if( $1->usage == FldIndex ) {
                    field_type_update($1, $1->usage, @1, true);
                  }
                }

        |       level_name CONSTANT is_global as const_value
                {
                  cbl_field_t& field = *$1;
                  if( field.level != 1 ) {
                    error_msg(@1, "%s must be an 01-level data item", field.name);
                    YYERROR;
                  }

                  field.attr |= constant_e;
                  if( $is_global ) field.attr |= global_e;
                  field.type = FldLiteralN;
		  field.data = build_real (float128_type_node, $const_value);
                  field.data.initial = string_of($const_value);

                  if( !cdf_value(field.name, cdfval_t($const_value)) ) {
                    error_msg(@1, "%s was defined by CDF", field.name);
                  }
                }
        |       level_name CONSTANT is_global as literalism[lit]
                {
                  cbl_field_t& field = *$1;
                  field.attr |= constant_e;
                  if( $is_global ) field.attr |= global_e;
                  field.type = FldLiteralA;
                  field.data.capacity = $lit.len;
                  field.data.initial  = $lit.data;
                  field.attr |= literal_attr($lit.prefix);
                  if( field.level != 1 ) {
                    error_msg(@lit, "%s must be an 01-level data item", field.name);
                    YYERROR;
                  }
                  if( !cdf_value(field.name, $lit.data) ) {
                    error_msg(@1, "%s was defined by CDF", field.name);
                  }
                  value_encoding_check(@lit, $1);
                }
        |       level_name CONSTANT is_global FROM NAME
                {
                  assert($1 == current_field());
                  const cdfval_t *cdfval = cdf_value($NAME);
                  if( !cdfval ) {
                    error_msg(@1, "%s was defined by CDF", $NAME);
                    YYERROR;
                  }
                  cbl_field_t& field = *$1;
                  field.attr |= ($is_global | constant_e);
                  field.data.capacity = cdfval->string ? strlen(cdfval->string)
                                                  : sizeof(field.data.value_of());
                  field.data.initial  = cdfval->string;
                  field.data = cdfval->number;
                  if( !cdf_value(field.name, *cdfval) ) {
                    error_msg(@1, "%s was defined by CDF", field.name);
                  }
                }

        |       LEVEL78 NAME[name] VALUE is value78[data]
                {
                  if( ! dialect_mf() ) {
                    dialect_error(@1, "level 78", "mf");
                    YYERROR;
                  }
                  struct cbl_field_t field = { 0, FldLiteralA, FldInvalid,
                                               constant_e, 0, 0, 78, nonarray,
                                               yylineno, "", 0, {}, *$data, NULL };
                  if( !namcpy(@name, field.name, $name) ) YYERROR;
                  if( field.data.initial ) {
                    field.attr |= quoted_e;
                    if( !cdf_value(field.name, field.data.initial) ) {
                      yywarn("%s was defined by CDF", field.name);
                    }
                  } else {
                    field.type = FldLiteralN;
                    field.data.initial = string_of(field.data.value_of());
                    if( !cdf_value(field.name, field.as_integer()) ) {
                      yywarn("%s was defined by CDF", field.name);
                    }
                  }
                  if( ($$ = field_add(@name, &field)) == NULL ) {
                    error_msg(@name, "failed level 78");
                    YYERROR;
                  }
                }

        |       LEVEL88 NAME /* VALUE */ NULLPTR
                {
                  struct cbl_field_t field = { 0,
                    FldClass, FldInvalid, 0, 0, 0, 88, nonarray, yylineno, "",
                    0, cbl_field_t::linkage_t(),
		    {}, NULL };
                  if( !namcpy(@NAME, field.name, $2) ) YYERROR;

                  auto fig = constant_of(constant_index(NULLS))->data.initial;
                  struct cbl_domain_t *domain = new cbl_domain_t[2];

                  domain[0] = cbl_domain_t(@NAME, false, strlen(fig), fig);

                  field.data.domain_as(domain);

                  if( ($$ = field_add(@2, &field)) == NULL ) {
                    error_msg(@NAME, "failed level 88");
                    YYERROR;
                  }
                  auto parent = cbl_field_of(symbol_at($$->parent));
                  if( parent->type != FldPointer ) {
                    error_msg(@NAME, "LEVEL 88 %s VALUE NULLS invalid for "
                             "%s %s, which is not a POINTER",
			    $$->name, parent->level_str(), parent->name);
                  }
                }
        |       LEVEL88 NAME VALUE domains
                {
                  struct cbl_field_t field = { 0,
                    FldClass, FldInvalid, 0, 0, 0, 88, nonarray, yylineno, "",
                    0, cbl_field_t::linkage_t(),
		    {}, NULL };
                  if( !namcpy(@NAME, field.name, $2) ) YYERROR;

                  struct cbl_domain_t *domain =
                    new cbl_domain_t[ domains.size() + 1];

                  std::copy(domains.begin(), domains.end(), domain);

                  field.data.domain_as(domain);
                  field.data.false_value_as($domains);
                  domains.clear();

                  if( ($$ = field_add(@2, &field)) == NULL ) {
                    error_msg(@NAME, "failed level 88");
                    YYERROR;
                  }
                }

        |       name66[alias] RENAMES name[orig]
                {
		  symbol_field_alias_end();
                  if( is_literal($orig) ) {
                    error_msg(@orig, "cannot RENAME '%s'", name_of($orig));
                    YYERROR;
                  }
                  if( !immediately_follows($orig) ) {
                    error_msg(@orig, "%s must immediately follow %s to RENAME it",
                             $alias, name_of($orig));
                    YYERROR;
                  }
                  if( $orig->occurs.ntimes() ) {
                    error_msg(@orig, "cannot RENAME table %s %s",
			      $orig->level_str(), name_of($orig));
                    YYERROR;
                  }
                  auto table = occurs_in($orig);
                  if( table ) {
                    error_msg(@orig, "cannot RENAME '%s' OF %s",
                             name_of($orig), table->name);
                    YYERROR;
                  }
                  if( ! $orig->rename_level_ok() ) {
                    error_msg(@orig, "cannot RENAME %s %s",
			      $orig->level_str(), name_of($orig));
                    YYERROR;
                  }
                  symbol_elem_t *orig = symbol_at(field_index($orig));
                  $$ = cbl_field_of(symbol_field_alias(orig, $alias));
		  symbol_field_location(field_index($$), @alias);
                }

        |       name66[alias] RENAMES name[orig] THRU name[thru]
                {
		  symbol_field_alias_end();
                  if( !immediately_follows($orig) ) {
                    error_msg(@orig, "RENAMES: %s must immediately follow %s",
                             $alias, name_of($orig));
                    YYERROR;
                  }
                  if( is_literal($orig) ) {
                    error_msg(@orig, "cannot RENAME '%s'", name_of($orig));
                    YYERROR;
                  }
                  if( is_literal($thru) ) {
                    error_msg(@thru, "cannot RENAME '%s'", name_of($thru));
                    YYERROR;
                  }
                  auto table = occurs_in($orig);
                  if( table ) {
                    error_msg(@orig, "cannot RENAME '%s' OF %s",
                             name_of($orig), table->name);
                    YYERROR;
                  }
                  table = occurs_in($thru);
                  if( table ) {
                    error_msg(@thru, "cannot RENAME '%s' OF %s",
                             name_of($thru), table->name);
                    YYERROR;
                  }
                  if( ! $orig->rename_level_ok() ) {
                    error_msg(@orig, "cannot RENAME %s %s",
			      $orig->level_str(), name_of($orig));
                    YYERROR;
                  }
                  if( $orig->has_subordinate($thru) ) {
                    error_msg(@orig, "cannot RENAME %s %s THRU %s %s "
                             "because %s is subordinate to %s",
			      $orig->level_str(), name_of($orig),
			      $thru->level_str(), name_of($thru),
                             name_of($thru), name_of($orig));
                    YYERROR;
                  }
                  auto not_ok = rename_not_ok($orig, $thru);
                  if( not_ok ) {
                    error_msg(@orig, "cannot RENAME %s %s THRU %s %s "
                             "because %s %s cannot be renamed",
			      $orig->level_str(), name_of($orig),
			      $thru->level_str(), name_of($thru),
			      not_ok->level_str(), name_of(not_ok));
                    YYERROR;
                  }
                  if( field_index($thru) <= field_index($orig) ) {
                    error_msg(@orig, "cannot RENAME %s %s THRU %s %s "
                             "because they're in the wrong order",
			      $orig->level_str(), name_of($orig),
			      $thru->level_str(), name_of($thru));
                    YYERROR;
                  }
                  symbol_elem_t *orig = symbol_at(field_index($orig));
                  symbol_elem_t *last = symbol_at(field_index($thru));
                  $$ = cbl_field_of(symbol_field_alias2(orig, last, $alias));
		  symbol_field_location(field_index($$), @alias);
                }

        |       level_name[field] data_clauses
                {
                  gcc_assert($field == current_field());
                  if( $data_clauses == value_clause_e ) { // only VALUE, no PIC
                    // Error unless VALUE is a figurative constant or (quoted) string.
                    if( $field->type != FldPointer &&
                        ! $field->has_attr(quoted_e) &&
                        normal_value_e == cbl_figconst_of($field->data.initial) )
                    {
                      error_msg(@field, "%s numeric VALUE %s requires PICTURE",
                               $field->name, $field->data.initial);
                    }
                    if( null_value_e == cbl_figconst_of($field->data.initial) ) {
                      // don't change the type
                      assert(FldPointer == $field->type);
                    } else {
                      // alphanumeric VALUE by itself implies alphanumeric type
                      assert(FldPointer != $field->type);
                      $field->type = FldAlphanumeric;
                      if( $field->data.initial ) {
                        $field->data.capacity = strlen($field->data.initial);
                      }
                    }
                  }

                  // Verify BLANK WHEN ZERO
                  if( $field->has_attr(blank_zero_e) ) {
                    switch($field->type) {
                    case FldNumericEdited:
                      if( $field->has_attr(signable_e) ) {
                        error_msg(@2,  "%s has 'S' in PICTURE, cannot be BLANK WHEN ZERO",
                                  $field->name, cbl_field_type_str($field->type) );
                      }
                      break;
                    default:
                      error_msg(@2,  "%s must be "
                                "NUMERIC DISPLAY or NUMERIC-EDITED, not %s",
                                $field->name, cbl_field_type_str($field->type) );
                    }
                    $field->data.picture = original_picture();
                  }

                  // SIGN clause valid only with "S" in picture
                  if( $field->type == FldNumericDisplay && !is_signable($field) ) {
                    static const size_t sign_attrs = leading_e | separate_e;
                    static_assert(sizeof(sign_attrs) == sizeof($field->attr),
                                  "size matters");

                    // remove inapplicable inherited sign attributes
                    size_t group_sign = group_attr($field) & sign_attrs;
                    $field->attr &= ~group_sign;

                    if( $field->attr & sign_attrs ) {
                      dbgmsg("%s:%d: %s", __func__, __LINE__, field_str($field));
                      error_msg(@field, "%s must be signed for SIGN IS",
                                $field->name );
                      YYERROR;
                    }
                  }

                  // Increase numeric display capacity by 1 for SIGN SEPARATE.
                  if( $field->type == FldNumericDisplay &&
                      is_signable($field) &&
                      $field->has_attr(separate_e) ){
                    $field->data.capacity++;
                  }

                  // Set Packed-Decimal capacity
                  if( $field->type == FldPacked ) {
                    $field->data.capacity = type_capacity($field->type,
                                                          $field->data.digits);
                  if( $field->attr & separate_e )
                    {
                    // This is a gentle kludge required by the the belated
                    // introduction of COMP-6, which is like COMP-3 but with no
                    // sign nybble.  The code in type_capacity assumes a sign
                    // nybble.
                    $field->data.capacity = ($field->data.digits+1)/2;
                    }
                  }

                  // Check COMP-5 capacity
		  // No capacity means no PICTURE, valid only for a (potential) group
                  if( $field->type == FldNumericBin5 && $field->data.capacity == 0 ) {
                    if(  has_clause ($data_clauses, usage_clause_e) &&
                        !has_clause ($data_clauses, picture_clause_e) ) {
			// invalidate until a child is born
		      $field->type = FldInvalid;
                    }
                  }

                  // Ensure signed initial VALUE is for signed numeric type
                  if( is_numeric($field) &&
		      $field->data.initial &&
		      $field->type != FldFloat )
		  {
                    switch( $field->data.initial[0] ) {
                    case '-':
                      if( !$field->has_attr(signable_e) ) {
                        error_msg(@field, "%s is unsigned but has signed VALUE '%s'",
                                 $field->name, $field->data.initial);
                      }
                    }
                  }

                  // Verify VALUE
                  $field->report_invalid_initial_value(@data_clauses);

                  // verify REDEFINES
                  auto parent = parent_of($field);
                  if( parent && $field->level == parent->level ) {
                    valid_redefine(@field, $field, parent); // calls yyerror
                  }
                }
                ;

literalism:     LITERAL { $$ = $1; }
        |       literalism[first] '&' LITERAL[second]
                {
                  $$ = $first;
                  literal_t& output($$);

                  output.len += $second.len;
                  output.data = reinterpret_cast<char*>(xrealloc(output.data,
                                                                 output.len + 1));
                  memcpy( output.data + $first.len, $second.data, $second.len );
                  output.data[output.len] = '\0';

                  if( $second.prefix[0] ) { strcpy(output.prefix, $second.prefix); }
                  if( ! $first.compatible_prefix($second) ) {
                    yywarn("dissimilar literals, '%s' prevails",
                          output.prefix);
                  }
                }
                ;

name66:         LEVEL66 NAME[alias]
                {
                  build_symbol_map();
		  if( ! symbol_field_alias_begin() ) {
		     error_msg(@alias, "no Level 01 record exists "
		              "for %s to redefine", $alias);
		  }
                  $$ = $alias;
                }
                ;

data_clauses:   data_clause
                {
                  if( $data_clause == redefines_clause_e ) {
                    auto parent = parent_of(current_field());
                    if( !parent ) {
                      error_msg(@1, "%s invalid REDEFINES",
                               current_field()->name);
                      YYERROR;
                    }
                    if( parent->occurs.ntimes() > 0 ) {
                      error_msg(@1, "%s cannot REDEFINE table %s",
                               current_field()->name,
                               parent->name);
                      YYERROR;
                    }
                  }
                }
        |       data_clauses data_clause {
                  const char *clause = "data";
                  switch($2) {
                  case occurs_clause_e:     clause = "OCCURS";    break;
                  case picture_clause_e:    clause = "PIC";       break;
                  case usage_clause_e:      clause = "USAGE";     break;
                  case value_clause_e:      clause = "VALUE";     break;
                  case global_clause_e:     clause = "GLOBAL";    break;
                  case external_clause_e:   clause = "EXTERNAL";  break;
                  case justified_clause_e:  clause = "JUSTIFIED"; break;
                  case redefines_clause_e:  clause = "REDEFINES"; break;
                  case blank_zero_clause_e: clause = "BLANK WHEN ZERO"; break;
                  case synched_clause_e:    clause = "SYNCHRONIZED"; break;
                  case sign_clause_e:       clause = "SIGN";      break;
                  case based_clause_e:      clause = "BASED";     break;
                  case same_clause_e:       clause = "SAME AS";   break;
                  case volatile_clause_e:   clause = "VOLATILE";  break;
                  case type_clause_e:       clause = "TYPE";      break;
                  case typedef_clause_e:    clause = "TYPEDEF";   break;
                  }
                  if( ($$ & $2) == $2 ) {
                    error_msg(@2, "%s clause repeated", clause);
                    YYERROR;
                  }

                  if( $data_clause == redefines_clause_e ) {
                    error_msg(@2, "REDEFINES must appear "
                             "immediately after LEVEL and NAME");
                    YYERROR;
                  }
                  cbl_field_t *field = current_field();
                  const int globex = (global_e | external_e);
                  if( (($$ | $2) & globex) == globex ) {
                    error_msg(@2, "GLOBAL and EXTERNAL specified");
                    YYERROR;
                  }

                  $$ |= $2;

                  // If any implied TYPE bits are on in addition to
                  // type_clause_e, they're in conflict.
                  static const size_t type_implies =
                    // ALIGNED clause not implemented
                    blank_zero_clause_e | justified_clause_e | picture_clause_e
                    | sign_clause_e | synched_clause_e | usage_clause_e;

                  if( type_clause_e < ($$ & (type_clause_e | type_implies)) ) {
                    if( $2 == type_clause_e ) {
                      error_msg(@2, "TYPE TO incompatible with ALIGNED, "
                              "BLANK WHEN ZERO, JUSTIFIED, PICTURE, SIGN, "
                              "SYNCHRONIZED, and USAGE");
                    } else {
                      error_msg(@2, "%s incompatible with TYPE TO", clause);
                    }
                    YYERROR;
                  }

                  if( ($$ & same_clause_e) == same_clause_e ) {
                    if( 0 < ($$ & ~same_clause_e) ) {
                      error_msg(@2, "%s %s SAME AS "
			      "precludes other DATA DIVISION clauses",
			      field->level_str(), field->name);
                      YYERROR;
                    }
                  }

                  if( is_numeric(field->type) && field->type != FldNumericDisplay ) {
                    if( $$ & sign_clause_e ) {
                      error_msg(@2, "%s is binary NUMERIC type, "
                               "incompatible with SIGN IS", field->name);
                    }
                  }

                  if( gcobol_feature_embiggen() ) {
                    if( field->is_binary_integer() && field->data.capacity == 4) {
                      auto redefined = symbol_redefines(field);
                      if( redefined && redefined->type == FldPointer ) {
                        if( yydebug ) {
                          yywarn("expanding %s size from %u bytes to %zu "
                                "because it redefines %s with USAGE POINTER",
                                field->name, field->size(),
                                (size_t)int_size_in_bytes(ptr_type_node),
                                redefined->name);
                        }
                        field->embiggen();
                      }
                    }
                  }

                  switch( field->type ) {
                  case FldFloat:
                    if( ($$ & picture_clause_e) == picture_clause_e ) {
		      error_msg(@2, "%s: FLOAT types do not allow PICTURE",
			       field->name);
                    }
                    break;
                  default:
                    break;
                  }

		  if( ! field->is_justifiable() ) {
		    error_msg(@2, "%s: %s is incompatible with JUSTIFIED",
			     field->name, 3 + cbl_field_type_str(field->type));
		  }
                }
                ;

data_clause:    any_length        { $$ = any_length_e; }
        |       based_clause      { $$ = based_clause_e; }
        |       blank_zero_clause { $$ = blank_zero_clause_e; }
        |       external_clause   { $$ = external_clause_e; }
        |       global_clause     { $$ = global_clause_e; }
        |       justified_clause  { $$ = justified_clause_e; }
        |       occurs_clause     { $$ = occurs_clause_e;
                  cbl_field_t *field = current_field();
                  switch( field->level ) {
                  case 1:
		    if( dialect_mf() ) break;
		    __attribute__((fallthrough));
                  case 77:
                  case 88:
		    error_msg(@$, "%s %s: invalid LEVEL for OCCURS",
			      field->level_str(), field->name );
                    break;
                  default:
                    assert( field->parent > 0 );
                  }
                }
        |       picture_clause    { $$ = picture_clause_e; }
        |       redefines_clause  { $$ = redefines_clause_e; }
        |       same_clause       { $$ = same_clause_e; }
        |       sign_clause       { $$ = sign_clause_e; }
        |       synched_clause    { $$ = synched_clause_e; }
        |       type_clause       { $$ = type_clause_e; }
        |       typedef_clause    { $$ = typedef_clause_e; }
        |       usage_clause      { $$ = usage_clause_e; }
        |       value_clause      { $$ = value_clause_e;
                  cbl_field_t *field = current_field();

                  if( field->type != FldAlphanumeric &&
                      field->data.initial && field->data.initial[0] )
                  {
                    // Embedded NULs are valid only in FldAlphanumeric, and are
                    // already handled.
                    if( strlen(field->data.initial) < field->data.capacity ) {
                      auto p = blank_pad_initial( field->data.initial,
                                                  strlen(field->data.initial),
                                                  field->data.capacity );
                      if( !p ) YYERROR;
                      field->data.initial = p;
                    }
                  }
		  const cbl_field_t *parent;
		  if( (parent = parent_has_value(field)) != NULL ) {
		    error_msg(@1, "VALUE invalid because group %s has VALUE clause",
		              parent->name);
		  }
                }
        |       volatile_clause      { $$ = volatile_clause_e; }
                ;

picture_clause: PIC signed nps[fore] nines nps[aft]
                {
                  cbl_field_t *field = current_field();
                  if( !field_type_update(field, FldNumericDisplay, @$) ) {
                    YYERROR;
                  }
                  ERROR_IF_CAPACITY(@PIC, field);
                  field->attr |= $signed;
                  field->data.capacity = type_capacity(field->type, $4);
                  field->data.digits = $4;
                  if( long(field->data.digits) != $4 ) {
                    error_msg(@2, "indicated size would be %ld bytes, "
                             "maximum data item size is %u",
                             $4, UINT32_MAX);
                  }

                  if( $fore && $aft ) { // leading and trailing P's
                    error_msg(@2, "PIC cannot have both leading and trailing P");
                    YYERROR;
                  }
                  if( $fore || $aft ) {
                    field->attr |= scaled_e;
                    field->data.rdigits = $fore? $fore : -$aft;
                  }
                  if( ! field->reasonable_capacity() ) {
                    error_msg(@2, "%s limited to capacity of %d (would need %u)",
			     field->name, MAX_FIXED_POINT_DIGITS, field->data.capacity);
                  }
                }

        |       PIC signed NINEV[left] nine[rdigits]
                {
                  cbl_field_t *field = current_field();
                  field->data.digits = $left + $rdigits;

                  if( field->is_binary_integer() ) {
                    field->data.capacity = type_capacity(field->type,
                                                         field->data.digits);
                  } else {
                    if( !field_type_update(field, FldNumericDisplay, @$) ) {
                      YYERROR;
                    }
                    ERROR_IF_CAPACITY(@PIC, field);
                    field->attr |= $signed;
                    field->data.capacity = field->data.digits;
                    field->data.rdigits = $rdigits;
                  }
                  if( ! field->reasonable_capacity() ) {
                    error_msg(@2, "%s limited to capacity of %d (would need %u)",
			     field->name, MAX_FIXED_POINT_DIGITS, field->data.capacity);
                  }
                }
        |       PIC signed NINEDOT[left] nine[rdigits]
                {
                  uint32_t size = $left + $rdigits;

                  cbl_field_t *field = current_field();
                  if( !field_type_update(field, FldNumericEdited, @$) ) {
                    YYERROR;
                  }
                  ERROR_IF_CAPACITY(@PIC, field);
                  field->attr |= $signed;
                  field->data.digits = size;
                  field->data.capacity = ++size;
                  field->data.rdigits = $rdigits;

                  if( ! field->reasonable_capacity() ) {
                    error_msg(@2, "%s limited to capacity of %d (would need %u)",
			     field->name, MAX_FIXED_POINT_DIGITS, field->data.capacity);
                  }
                }

        |       PIC alphanum_pic[size]
                {
                  cbl_field_t *field = current_field();

		  if( field->type == FldNumericBin5 &&
		      field->data.capacity == 0  &&
		      dialect_mf() )
		  { // PIC X COMP-X or COMP-9
		    if( ! field->has_attr(all_x_e) ) {
		      error_msg(@2, "COMP PICTURE requires all X's or all 9's");
                      YYERROR;
		    }
		  } else {
                    if( !field_type_update(field, FldAlphanumeric, @$) ) {
                      YYERROR;
                    }
		  }
                  assert(0 < $size);
                  if( field->data.initial != NULL ) {
                    if( 0 < field->data.capacity &&
                            field->data.capacity < uint32_t($size) ) {
                      auto p = blank_pad_initial( field->data.initial,
                                                  field->data.capacity, $size );
                      if( !p ) YYERROR;
                      field->data.initial = p;
                    }
                  }

                  field->data.capacity = $size;
                  field->data.picture = NULL;

                  if( false ) dbgmsg("PIC alphanum_pic[size]:%d: %s",
                                      field->line, field_str(field));
                }

        |       PIC numed[picture]
                {
                  cbl_field_t *field = current_field();
                  if( !field_type_update(field, FldNumericEdited, @$) ) {
                    YYERROR;
                  }
                  ERROR_IF_CAPACITY(@PIC, field);
                  if( !is_numeric_edited($picture) ) {
                    error_msg(@picture, numed_message);
                    YYERROR;
                  }
                  field->data.picture = $picture;
                  field->data.capacity =  length_of_picture($picture);
                  field->data.digits   =  digits_of_picture($picture, false);
                  field->data.rdigits  = rdigits_of_picture($picture);
                  if( is_picture_scaled($picture) ) field->attr |= scaled_e;
                }

        |       PIC ALPHED[picture]
                {
                  bool is_alpha_edited( const char picture[] );

                  cbl_field_t *field = current_field();
                  ERROR_IF_CAPACITY(@PIC, field);
                  field->data.capacity = length_of_picture($picture);
                  field->data.picture = $picture;

                  // In case the lexer guesses wrong.
                  cbl_field_type_t type = is_numeric_edited($picture)?
                                          FldNumericEdited : FldAlphaEdited;
                  if( !field_type_update(field, type, @$) ) {
                    YYERROR;
                  }

                  switch( type ) {
                  case FldNumericEdited:
                    field->data.digits   =  digits_of_picture($picture, false);
                    field->data.rdigits  = rdigits_of_picture($picture);
                    if( is_picture_scaled($picture) ) field->attr |= scaled_e;
                    break;
                  case FldAlphaEdited:
                    if( !is_alpha_edited(field->data.picture) ) {
                      error_msg(@picture, "invalid picture for Alphanumeric-edited");
                      YYERROR;
                    }
                    break;
                  default:
                    gcc_unreachable();
                  }
                }
                ;

alphanum_pic:   alphanum_part {
                  current_field()->set_attr($1.attr);
                  $$ = $1.nbyte;
                }
        |       alphanum_pic alphanum_part
                {
		  auto field = current_field();
		  dbgmsg("%s has %s against %s",
			 field->name, field_attr_str(field),
			 cbl_field_attr_str($2.attr));

		  if( ! field->has_attr($2.attr) ) {
                    field->clear_attr(all_ax_e); // clears 2 bits
                  }
                  $$ += $2.nbyte;

		  dbgmsg("%s attrs: %s", field->name, field_attr_str(field));
                }
                ;
alphanum_part:  ALNUM[picture] count
                {
                  $$.attr = uniform_picture($picture);
                  $$.nbyte = strlen($picture);
		  auto count($count);
                  if( count > 0 ) {
                    --count;
                    $$.nbyte += count; // AX9(3) has count 5
                  }
		  if( count < 0 ) {
		    error_msg(@2, "PICTURE count '(%d)' is negative", count );
		    YYERROR;
		  }
                }
                ;

signed:         %empty           { $$ = 0; }
        |       'S'              { $$ = signable_e; }
                ;

nps:            %empty           { $$ = 0; }
        |       PIC_P            { $$ = $1; }
                ;

nine:           %empty           { $$ = 0; }
        |       nines
                {
                  $$ = $1;
		  if( $$ == 0 ) {
		    error_msg(@1, "'(0)' invalid in PICTURE (ISO 2023 13.18.40.3)");
		  }
                }
                ;
nines:		NINES
	|	nines NINES { $$ = $1 + $2; }
		;

count:          %empty           { $$ = 0; }
        |       '(' NUMSTR ')'
                {
                  REAL_VALUE_TYPE rn = numstr2i($NUMSTR.string, $NUMSTR.radix);
                  $$ = real_to_integer (&rn);
		  if( $$ == 0 ) {
		    error_msg(@2, "'(0)' invalid in PICTURE (ISO 2023 13.18.40.3)");
		  }
                }
	|	'(' NAME ')'
                {
		  auto value = cdf_value($NAME);
		  if( ! (value && value->is_numeric()) ) {
		    error_msg(@NAME, "PICTURE '(%s)' requires a CONSTANT value", $NAME );
		    YYERROR;
		  }
		  int nmsg = 0;
		  auto e = symbol_field(PROGRAM, 0, $NAME);
		  if( e ) { // verify not floating point with nonzero fraction
		    auto field = cbl_field_of(e);
		    assert(is_literal(field));
		    REAL_VALUE_TYPE vi;
		    real_from_integer (&vi, VOIDmode, field->as_integer(), SIGNED);
		    if( !real_identical (TREE_REAL_CST_PTR (field->data.value_of()),
				         &vi) ) {
		      nmsg++;
		      error_msg(@NAME, "invalid PICTURE count '(%s)'",
				field->data.initial );
		    }
		  }
		  $$ = value->as_number();
		  if( $$ <= 0 && !nmsg) {
		    error_msg(@NAME, "invalid PICTURE count '(%s)'", $NAME );
		  }
                }
                ;

numed:          NUMED
        |       NUMED_CR
        |       NUMED_DB
                ;

usage_clause:   usage_clause1[type]
                {
                  cbl_field_t *field = current_field();
                  cbl_field_type_t type = static_cast<cbl_field_type_t>($type);
                  if( ! field_type_update(field, type, @$, true) ) {
                    YYERROR;
                  }
                }
                ;
usage_clause1:  usage COMPUTATIONAL[comp]   native
                {
                  bool infer = true;
                  cbl_field_t *field = current_field();

                  // Some binary types have defined capacity;
                  switch($comp.type) {
                  // COMPUTATIONAL and COMP-5 rely on PICTURE.
                  case FldNumericBinary:
                    field->attr |= big_endian_e;
                    __attribute__((fallthrough));
                  case FldNumericBin5:
		    // If no capacity yet, then no picture, infer $comp.capacity.
		    // If field has capacity, ensure USAGE is compatible.
		    if( field->data.capacity > 0 ) { // PICTURE before USAGE
		      infer = false;
		      switch( field->type ) {
		      case FldAlphanumeric:   // PIC X COMP-5 or COMP-X
			assert( field->data.digits == 0 );
			assert( field->data.rdigits == 0 );
		        if( dialect_mf() ) {
                          field->type = $comp.type;
			  field->clear_attr(signable_e);
		        } else {
			  error_msg(@comp, "numeric USAGE invalid "
				   "with Alpnanumeric PICTURE");
			  YYERROR;
		        }
                        break;
		      case FldNumericDisplay: // PIC 9 COMP-5 or COMP-X
		        if( $comp.capacity == 0xFF ) { // comp-x is a bit like comp-5
			  assert( field->data.digits == field->data.capacity );
		          if( ! dialect_mf() ) {
				  dialect_error(@1, "COMP-X", "mf");
		          }
			}
                        field->type = $comp.type;
                        field->data.capacity = type_capacity(field->type,
                                                             field->data.digits);
		        break;
		      default: break;
                      }
		    }
                    break;
		  case FldPacked: // comp-6 is unsigned comp-3
		    assert(! $comp.signable);  // else PACKED_DECIMAL from scanner
		    field->attr |= separate_e;
		    if( ! dialect_mf() ) {
		      dialect_error(@1, "COMP-6", "mf");
		    }
                    if( field->type == FldNumericDisplay ) {// PICTURE before USAGE
                      infer = false;
                      assert(field->data.capacity > 0);
                      field->type = $comp.type;
                      field->data.capacity = type_capacity(field->type,
                                                           field->data.digits);
                    }
		    break;
                  default:
                    break;
                  }

                  if( infer ) {
                    if( $comp.capacity > 0 ) {
                      if( field->data.capacity > 0 ) {
                        error_msg(@comp, "%s is BINARY type, incompatible with PICTURE",
                               field->name);
                        YYERROR;
                      }
                      field->data.capacity = $comp.capacity;
                      field->type = $comp.type;
                      if( $comp.signable ) {
                        field->attr = (field->attr | signable_e);
                      }
                    }
                  }
                  $$ = $comp.type;
                }
        |       usage DISPLAY         native { $$ = FldDisplay; }
        |       usage PACKED_DECIMAL  native { $$ = FldPacked; }
        |       usage PACKED_DECIMAL  with NO SIGN
		{
                  cbl_field_t *field = current_field();
		  if( field->data.capacity > 0 &&
		      field->type != FldNumericDisplay) {
		    error_msg(@2, "%s PICTURE is incompatible with USAGE PACKED DECIMAL",
			     field->name);
		    YYERROR;
		  }
		  field->clear_attr(separate_e);
		  field->clear_attr(signable_e);
                  if( field->type == FldNumericDisplay ) {// PICTURE before USAGE
                    assert(field->data.capacity > 0);
                    field->data.capacity = type_capacity(FldPacked,
                                                         field->data.digits);
                  }
		  $$ = field->type = FldPacked;
		}
        |       usage INDEX                  {
                  $$ = symbol_field_index_set( current_field() )->type;
                }
                // We should enforce data/code pointers with a different type.
        |       usage POINTER
                {
                  $$ = FldPointer;
                  auto field = current_field();
                  auto redefined = symbol_redefines(field);

		  if( $POINTER ) {
		    field->set_attr($POINTER);
		  }
                  if( gcobol_feature_embiggen() && redefined &&
                      is_numeric(redefined->type) && redefined->size() == 4) {
                    // For now, we allow POINTER to expand a 32-bit item to 64 bits.
                    field->data.capacity = int_size_in_bytes(ptr_type_node);
                    dbgmsg("%s: expanding #%zu %s capacity %u => %u", __func__,
                          field_index(redefined), redefined->name,
                          redefined->data.capacity, field->data.capacity);

                    redefined->embiggen();

                    if( redefined->data.initial ) {
                      auto s = xasprintf( "%s    ", redefined->data.initial);
                      std::replace(s, s + strlen(s), '!', char(0x20));
                      redefined->data.initial = s;
                    }
                  }
                }
        |       usage POINTER TO error
                {
                  cbl_unimplemented("POINTER TO");
                  $$ = FldPointer;
                }
		;

value_clause:   VALUE all LITERAL[lit] {
                  cbl_field_t *field = current_field();
                  field->data.initial  = $lit.data;
                  field->attr |= literal_attr($lit.prefix);
                  // The __gg__initialize_data routine needs to know that VALUE is a
                  // quoted literal. This is critical for NumericEdited variables
                  field->attr |= quoted_e;

                  if( field->data.capacity == 0 ) {
                    field->data.capacity = $lit.len;
                  } else {
                    if( $all ) {
                      field_value_all(field);
                    } else {
                      if( $lit.len < field->data.capacity ) {
                        auto p = blank_pad_initial( $lit.data, $lit.len,
                                                    field->data.capacity );
                        if( !p ) YYERROR;
                        field->data.initial = p;
                      }
                    }
                  }
                  value_encoding_check(@lit, field);
                }
        |       VALUE all cce_expr[value] {
                  cbl_field_t *field = current_field();
                  auto orig_str = original_number();
		  REAL_VALUE_TYPE orig_val;
		  real_from_string3 (&orig_val, orig_str,
				     TYPE_MODE (float128_type_node));
                  char *initial = NULL;

                  if( real_identical (&orig_val, &$value) ) {
                    initial = orig_str;
                    pristine_values.insert(initial);
                  } else {
                    initial = string_of($value);
                    gcc_assert(initial);
                  }

                  char decimal = symbol_decimal_point();
                  std::replace(initial, initial + strlen(initial), '.', decimal);

                  field->data.initial = initial;
                  field->data = build_real (float128_type_node, $value);

                  if( $all ) field_value_all(field);
                }
        |       VALUE all reserved_value[value]
                {
                  if( $value != NULLS ) {
                    auto fig = constant_of(constant_index($value));
                    current_field()->data.initial = fig->data.initial;
                  }
                }
        |       /* VALUE is */ NULLPTR
                {
                    auto fig = constant_of(constant_index(NULLS));
                    current_field()->data.initial = fig->data.initial;
                }
        |       VALUE error
                {
                  error_msg(@2, "no valid VALUE supplied");
                }
                ;

global_clause:  is GLOBAL
                {
                  cbl_field_t *field = current_field();
                  field->attr |= (field->attr | global_e);
                }
                ;
external_clause: is EXTERNAL
                {
                  cbl_field_t *field = current_field();
                  field->attr |= (field->attr | external_e);
                }
                ;

justified_clause: is JUSTIFIED
                {
                  cbl_field_t *field = current_field();
                  field->attr |= rjust_e;
                }
                ;

redefines_clause: REDEFINES NAME[orig]
                {
                  struct symbol_elem_t *e = field_of($orig);
                  if( !e ) {
                    error_msg(@2, "REDEFINES target not defined");
                    YYERROR;
                  }
                  cbl_field_t *field = current_field();
                  cbl_field_t *orig = cbl_field_of(e);
		  if( orig->has_attr(filler_e) ) {
                    error_msg(@2, "%s may not REDEFINE %s",
                            field->name, orig->name);
		  }
                  cbl_field_t *super = symbol_redefines(orig);
                  if( super ) {
                    error_msg(@2, "%s may not REDEFINE %s, "
                            "which redefines %s",
                            field->name, orig->name, super->name);
                  }
                  if( field->level != orig->level ) {
                    error_msg(@2, "cannot redefine %s %s as %s %s "
                             "because they have different levels",
			    orig->level_str(), name_of(orig),
			    field->level_str(), name_of(field));
                  }
		  // ISO 13.18.44.3
		  auto parent( symbol_index(e) );
		  auto p = std::find_if( symbol_elem_of(orig) + 1,
					 symbol_elem_of(field),
					 [parent, level = field->level]( const auto& elem ) {
					   if( elem.type == SymField ) {
					     auto f = cbl_field_of(&elem);
					     return
					       f->level  == level &&
					       f->parent != parent;
					   }
					   return false;
					 } );
		  if( p != symbol_elem_of(field) ) {
		    auto mid( cbl_field_of(p) );
                    error_msg(@2, "cannot redefine %s %s as %s %s "
			    "because %s %s intervenes",
			    orig->level_str(), name_of(orig),
			    field->level_str(), name_of(field),
			    mid->level_str(), name_of(mid));
                  }

                  if( valid_redefine(@2, field, orig) ) {
                    /*
                     * Defer "inheriting" the parent's description until the
                     * redefine is complete.
                     */
                    current_field()->parent = symbol_index(e);
                  }
                }
                ;

any_length:     ANY LENGTH
                { cbl_field_t *field = current_field();
                  if( field->attr & any_length_e ) {
                    error_msg(@1, "ANY LENGTH already set");
                  }
                  if( ! (field->level == 1 &&
                         current_data_section == linkage_datasect_e &&
                         (1 < current.program_level() ||
			      current.program()->is_function())) ) {
                    error_msg(@1, "ANY LENGTH valid only for 01 "
                            "in LINKAGE SECTION of a function or contained program");
                    YYERROR;
                  }
                  field->attr |= any_length_e;
                }
                ;

based_clause:   BASED
                { cbl_field_t *field = current_field();
                  if( field->attr & based_e ) {
                    error_msg(@1, "BASED already set");
                  }
                  field->attr |= based_e;
                }
                ;

blank_zero_clause: blank_when_zero
                { cbl_field_t *field = current_field();
                  // the BLANK WHEN ZERO clause defines the item as numeric-edited.
                  if( !field_type_update(field, FldNumericEdited, @1) ) {
                    YYERROR;
                  }
                  field->attr |= blank_zero_e;
                }
                ;
blank_when_zero:
                BLANK WHEN ZERO
        |       BLANK      ZERO
                ;

synched_clause: SYNCHRONIZED
        |       SYNCHRONIZED LEFT
        |       SYNCHRONIZED RIGHT
                ;

same_clause:    SAME AS name
                {
                  cbl_field_t *field = current_field(), *other = $name;
                  if( other->occurs.ntimes() > 0 ) {
                    error_msg(@name, "SAME AS %s: cannot have OCCURS",
                             other->name); // 13.18.49.2,P5
                    YYERROR;
                  }
                  if( field->level == 77 and !is_elementary(other->type) ) {
		    // ISO 2023 13.18.49.2,P8
                    error_msg(@name, "%s %s SAME AS %s: must be elementary",
			    field->level_str(), field->name, other->name);
                    YYERROR;
                  }

                  if( (other->attr & (sign_clause_e | usage_clause_e)) > 0 ) {
                    error_msg(@name, "%s: source of SAME AS cannot have "
                               "SIGN or USAGE clause", other->name);
                    YYERROR;
                  }
                  if( other->usage == FldGroup ) {
                    error_msg(@name, "%s: source of SAME AS cannot have "
                               "GROUP-USAGE clause", other->name);
                    YYERROR;
                  }
                  if( other->has_attr(constant_e ) ) {
                    error_msg(@name, "%s: source of SAME AS cannot "
                               "be constant", other->name);
                    YYERROR;
                  }
                  if( field->parent == field_index(other) ) {
                    error_msg(@name, "%s: SAME AS uses "
                             "its own parent %s", field->name, other->name);
                    YYERROR;
                  }

                  auto e = symbol_field_same_as( field, other );
                  symbol_field_location( symbol_index(e), @name );
                }
                ;

sign_clause:    sign_is sign_leading sign_separate
                {
                  cbl_field_t *field = current_field();
                  if( $sign_leading ) {
                    field->attr |= leading_e;
                  } else {
                    field->attr &= ~size_t(leading_e); // turn off in case inherited
                    field->attr |= signable_e;
                  }
                  if( $sign_separate ) field->attr |= separate_e;
                }
                ;
sign_is:        %empty
        |       SIGN is
                ;
sign_leading:   LEADING  { $$ = true; }
        |       TRAILING { $$ = false; }
                ;
sign_separate:  %empty              { $$ = false; }
        |       SEPARATE CHARACTER  { $$ = true; }
        |       SEPARATE            { $$ = true; }
                ;

/*
 * "The effect of the TYPE clause is as though the data description identified
 *  by type-name-1 had been coded in place of the TYPE clause, excluding the
 *  level-number, name, alignment, and the GLOBAL, SELECT WHEN, and TYPEDEF
 *  clauses specified for type-name-1;"
 *
 * The essential characteristics of a type, which is identified by its
 * type-name, are the:
 *   — relative positions and lengths of the elementary items
 *   — ALIGNED clause
 *   — BLANK WHEN ZERO clause
 *   — JUSTIFIED clause
 *   — PICTURE clause
 *   — SIGN clause
 *   — SYNCHRONIZED clause
 *   — USAGE clause
 */
type_clause: TYPE to typename
                {
                  cbl_field_t *field = current_field();
                  if( $typename ) {
                    auto e = symbol_field_same_as(field, $typename);
		    symbol_field_location( symbol_index(e), @typename );
                  }
                }
        |       USAGE is typename
                {
                  if( ! dialect_mf() ) {
                    dialect_error(@typename, "USAGE TYPENAME", "mf");
                    YYERROR;
                  }
                  cbl_field_t *field = current_field();
                  if( $typename ) {
                    auto e = symbol_field_same_as(field, $typename);
		    symbol_field_location( symbol_index(e), @typename );
                  }
                }
                ;

typedef_clause: is TYPEDEF strong
                {
                  cbl_field_t *field = current_field();
                  switch( field->level ) {
                  case 1: case 77: break;
                  default:
                    error_msg(@2, "%s %s IS TYPEDEF must be level 01",
			    field->level_str(), field->name);
                  }
                  field->attr |= typedef_e;
                  if( $strong ) field->attr |= strongdef_e;
                  if( ! current.typedef_add(field) ) {
                    auto prior = current.has_typedef(field);
                    assert(prior);
                    error_msg(@2, "%s %s IS TYPEDEF is not unique "
                             "(see %s, line %d)",
			    field->level_str(), field->name,
			    prior->name, prior->line);
                  }
                }
                ;

volatile_clause:
                VOLATILE
                {
                  if( dialect_ibm() ) {
                    yywarn("VOLATILE has no effect");
                  } else {
                    dialect_error(@1, "VOLATILE", "ibm");
                  }
                }
                ;

procedure_div:  %empty {
		  if( !procedure_division_ready(@$, NULL, NULL) ) YYABORT;
                }
        |       PROCEDURE_DIV '.' {
                  if( !procedure_division_ready(@$, NULL, NULL) ) YYABORT;
                } declaratives sentences
        |       PROCEDURE_DIV procedure_args '.' declaratives sentences
        |       PROCEDURE_DIV procedure_args '.'
                ;

procedure_args: USING procedure_uses[args]
                {
                  if( !procedure_division_ready(@args, NULL, $args) ) YYABORT;
                }
        |       USING procedure_uses[args] RETURNING name[ret]
                {
                  if( !procedure_division_ready(@ret, $ret, $args) ) YYABORT;
                  if( ! $ret->has_attr(linkage_e) ) {
                    error_msg(@ret, "RETURNING %s is not defined in LINKAGE SECTION",
			      $ret->name);
                  }
                }
        |                                  RETURNING name[ret]
                {
                  if( !procedure_division_ready(@ret, $ret, NULL) ) YYABORT;
                  if( ! $ret->has_attr(linkage_e) ) {
                    error_msg(@ret, "RETURNING %s is not defined in LINKAGE SECTION",
			      $ret->name);
                  }
                }
                ;
procedure_uses: procedure_use { $$ = new ffi_args_t($1); }
        |       procedure_uses procedure_use { $$->push_back($2); }
                ;
procedure_use:  optional scalar {
                  $$ = new cbl_ffi_arg_t(by_default_e, $scalar);
                  $$->optional = $optional;
                  $$->validate(); // produces message
                }
        |       by REFERENCE optional scalar {
                  $$ = new cbl_ffi_arg_t(by_reference_e, $scalar);
                  $$->optional = $optional;
                  $$->validate(); // produces message
                }
        |       by CONTENT error { // no "by content" in procedure definition
                    $$ = new cbl_ffi_arg_t(by_content_e,
                                           new_reference(literally_zero));
                }
        |       by VALUE by_value_arg[arg] {
                  $$ = new cbl_ffi_arg_t(by_value_e, $arg);
                  $$->validate(); // produces message
                }
                ;
by_value_arg:   scalar
        |       LITERAL  { $$ = new_reference(new_literal($1, quoted_e)); }
        |       reserved_value
                {
                  $$ = new_reference(constant_of(constant_index($1)));
                }
                ;

declaratives:   %empty
        |       DECLARATIVES '.'
                <label>{
                  current.enabled_exception_cache = enabled_exceptions;
                  enabled_exceptions.clear();
                  current.doing_declaratives(true);
                  $$ = label_add(LblString, "_end_declaratives", 0);
                  assert($$);
                  parser_label_goto($$);
                } [label]
                sentences END DECLARATIVES '.'
                {
                  current.doing_declaratives(false);
                  /* TODO: if( intradeclarative_reference() ) yyerror;
                   * Test also at paragraph_reference, for non-forward
                   * reference with good line numbers.  See
                   * utilcc::procedures_t and ambiguous_reference().  At this
                   * point, no reference should pick up anything except a
                   * forward reference, because we haven't yet begun to parse
                   * nondeclarative procedures.
                   */
                  parser_label_label($label);
                  enabled_exceptions = current.enabled_exception_cache;
                  current.enabled_exception_cache.clear();
		  ast_enter_section(implicit_section());
                }
                ;

sentences:      sentence {
		  ast_first_statement(@1);
		  symbol_temporaries_free();
		}
	|	section_name
        |       paragraph_name[para] '.'
                {
                  location_set(@para);
                  cbl_label_t *label = label_add(@para, LblParagraph, $para);
                  if( !label ) {
                    YYERROR;
                  }
                  ast_enter_paragraph(label);
                  current.new_paragraph(label);
                  apply_declaratives();
                }
        |       sentences sentence
		{ // sentences might not be sentence
		  ast_first_statement(@2);
		  symbol_temporaries_free();
		}
        |       sentences section_name
        |       sentences paragraph_name[para] '.'
                {
                  location_set(@para);
                  cbl_label_t *label = label_add(@para, LblParagraph, $para);
                  if( !label ) {
                    YYERROR;
                  }
                  ast_enter_paragraph(label);
                  current.new_paragraph(label);
                  apply_declaratives();
                }
                ;
paragraph_name: NAME
        |       NUMSTR { $$ = $1.string; }
		;

sentence:       statements  '.'
        |       statements  YYEOF
                {
                  if( ! goodnight_gracie() ) {
                    YYABORT;
                  }
                  if( nparse_error > 0 ) YYABORT;
                  YYACCEPT;
                }
        |       program END_SUBPROGRAM namestr[name] '.'
                { // a contained program (no prior END PROGRAM) is a "sentence"
                  const cbl_label_t *prog = current.program();
                  assert(prog);
                  const char *name = string_of($name);
                  if( !name || 0 != strcasecmp(prog->name, name) ) {
                    error_msg(@name,  "END PROGRAM '%s' does not match PROGRAM-ID '%s'",
                              name? name : $name.data, prog->name);
                    YYERROR;
                  }

                  std::set<std::string> externals = current.end_program();
                  if( !externals.empty() ) {
                    for( const auto& name : externals ) {
                      yywarn("%s calls external symbol '%s'",
                             prog->name, name.c_str());
                    }
                    YYERROR;
                  }
                  // pointer still valid because name is in symbol table
                  ast_end_program(prog->name);
                }
        |       program YYEOF
                { // a contained program (no prior END PROGRAM) is a "sentence"
                  if( nparse_error > 0 ) YYABORT;
                  do {
		    if( ! goodnight_gracie() ) YYABORT; // no recovery
                  } while( current.program_level() > 0 );
                  YYACCEPT;
                }
                ;

statements:                statement { $$ = $1; }
        |       statements statement { $$ = $2; }
                ;

statement:      error {
                  if( current.declarative_section_name() ) {
		    error_msg(@1, "missing END DECLARATIVES or SECTION name",
			      nparse_error);
                    YYABORT;
                  }
                  if( max_errors_exceeded(nparse_error) ) {
                    error_msg(@1, "max errors %d reached", nparse_error);
                    YYABORT;
                  }
                }
        |       accept          { $$ =  ACCEPT; }
        |       add             { $$ =  ADD; }
        |       allocate        { $$ =  ALLOCATE; }
        |       alter           { $$ =  ALTER; }
        |       call            { $$ =  CALL; }
        |       cancel          { $$ =  CANCEL; }
        |       close           { $$ =  CLOSE; }
        |       compute         { $$ =  COMPUTE; }
        |       continue_stmt   { $$ =  CONTINUE; }
        |       delete          { $$ =  DELETE; }
        |       display         { $$ =  DISPLAY; }
        |       divide          { $$ =  DIVIDE; }
        |       entry           { $$ =  ENTRY; }
        |       evaluate        { $$ =  EVALUATE; }
        |       exit            { $$ =  EXIT; }
        |       free            { $$ =  FREE; }
        |       go_to           { $$ =  GOTO; }
        |       if_stmt         { $$ =  IF; }
        |       initialize      { $$ =  INITIALIZE; }
        |       inspect         { $$ =  INSPECT; }
        |       merge           { $$ =  MERGE; }
        |       move            { $$ =  MOVE; }
        |       multiply        { $$ =  MULTIPLY; }
        |       open            { $$ =  OPEN; }
        |       return_stmt     { $$ =  RETURN; }
        |       perform         { $$ =  PERFORM; }
        |       raise           { $$ =  RAISE; }
        |       read            { $$ =  READ; }
        |       release         { $$ =  RELEASE; }
        |       resume          { $$ =  RESUME; }
        |       rewrite         { $$ =  REWRITE; }
        |       search          { $$ =  SEARCH; }
        |       set             { $$ =  SET; }
        |       sort            { $$ =  SORT; }
        |       start           { $$ =  START; }
        |       stop            { $$ =  STOP; }
        |       string          { $$ =  STRING_kw; }
        |       subtract        { $$ =  SUBTRACT; }
        |       unstring        { $$ =  UNSTRING; }
        |       write           { $$ =  WRITE; }
                ;

		/*
		 * ISO defines ON EXCEPTION only for Format 3 (screen). We
		 * implement extensions defined by MF and Fujitsu (and us) to
		 * use ACCEPT to interact with the command line and the
		 * environment.
		 *
		 * ISO ACCEPT and some others are implemented in accept_body,
		 * before the parser sees any ON EXCEPTION.  In those cases
		 * accept_body returns accept_done_e to denote that the
		 * statement has been handled.  If ON EXCEPTION is then parsed,
		 * it's an error.  Otherwise, accept_body returns something
		 * else, and the relevant parser_accept_foo function is called
		 * in the "accept" action.
		 */
accept:         accept_body end_accept {
		  cbl_field_t *argi = register_find("_ARGI");
		  switch( $accept_body.func ) {
		  case accept_done_e:
		    break;
		  case accept_command_line_e:
		    if( $1.from->field == NULL ) { // take next command-line arg
		      parser_accept_command_line(*$1.into, argi, NULL, NULL);
		      cbl_num_result_t tgt { truncation_e, argi };
		      parser_add2(tgt, literally_one);	// increment argi
		    } else if( $1.from->field == argi ) {
		      parser_move(*$1.into, *$1.from);
		    } else {
		      parser_accept_command_line(*$1.into, *$1.from, NULL, NULL);
		    }
		    break;
		  case accept_envar_e:
		    parser_accept_envar(*$1.into, *$1.from, NULL, NULL);
		    break;
		  }
		}
	|	accept_body accept_excepts[ec] end_accept {
		  cbl_field_t *argi = register_find("_ARGI");
		  switch( $accept_body.func ) {
		  case accept_done_e:
		    error_msg(@ec, "ON EXCEPTION valid only "
			    "with ENVIRONMENT or COMAMND-LINE(n)");
		    break;
		  case accept_command_line_e:
		    if( $1.from->field == NULL ) { // take next command-line arg
		      parser_accept_command_line(*$1.into, argi,
						 $ec.on_error, $ec.not_error);
		      cbl_num_result_t tgt { truncation_e, argi };
		      parser_add2(tgt, literally_one);	// increment argi
		    } else if( $1.from->field == argi ) {
		      parser_move(*$1.into, *$1.from);
		      if( $ec.on_error || $ec.not_error ) {
			error_msg(@ec, "ON EXCEPTION valid only "
				"with ENVIRONMENT or COMAMND-LINE(n)");
		      }
		    } else {
		      parser_accept_command_line(*$1.into, *$1.from,
						 $ec.on_error, $ec.not_error);
		    }
		    break;
		  case accept_envar_e:
		    parser_accept_envar(*$1.into, *$1.from,
					$ec.on_error, $ec.not_error);
		    break;
		  }
		}
                ;
end_accept:     %empty %prec ACCEPT
        |       END_ACCEPT
                ;

accept_body:    accept_refer
                {
		  $$.func = accept_done_e;
                  parser_accept(*$1, CONSOLE_e);
                }
        |       accept_refer FROM DATE
                {
		  $$.func = accept_done_e;
                  if( $1->is_reference() ) {
                    error_msg(@1, "subscripts are unsupported here");
                    YYERROR;
                  }
                  parser_accept_date_yymmdd($1->field);
                }
        |       accept_refer FROM DATE YYYYMMDD
                {
		  $$.func = accept_done_e;
                  if( $1->is_reference() ) {
                    error_msg(@1, "subscripts are unsupported here");
                    YYERROR;
                  }
                  parser_accept_date_yyyymmdd($1->field);
                }
        |       accept_refer FROM DAY
                {
		  $$.func = accept_done_e;
                  if( $1->is_reference() ) {
                    error_msg(@1, "subscripts are unsupported here");
                    YYERROR;
                  }
                  parser_accept_date_yyddd($1->field);
                }
        |       accept_refer FROM DAY YYYYDDD
                {
		  $$.func = accept_done_e;
                  if( $1->is_reference() ) {
                    error_msg(@1, "subscripts are unsupported here");
                    YYERROR;
                  }
                  parser_accept_date_yyyyddd($1->field);
                }
        |       accept_refer FROM DAY_OF_WEEK
                {
		  $$.func = accept_done_e;
                  if( $1->is_reference() ) {
                    error_msg(@1, "subscripts are unsupported here");
                    YYERROR;
                  }
                  parser_accept_date_dow($1->field);
                }

        |       accept_refer FROM TIME
                {
		  $$.func = accept_done_e;
                  if( $1->is_reference() ) {
                    error_msg(@1, "subscripts are unsupported here");
                    YYERROR;
                  }
                  parser_accept_date_hhmmssff($1->field);
                }
        |       accept_refer FROM acceptable
                {
		  cbl_field_t *argc = register_find("_ARGI");
		  switch( $acceptable->id ) {
		  case ARG_NUM_e:
		    $$.func = accept_command_line_e;
		    $$.into = $1;
		    $$.from = new_reference(argc);
		    break;
		  case ARG_VALUE_e:
		    $$.func = accept_command_line_e;
		    $$.into = $1;
		    $$.from = cbl_refer_t::empty();
		    break;
		  default:
		    $$.func = accept_done_e;
		    parser_accept( *$1, $acceptable->id );
		  }
                }
        |       accept_refer FROM ENVIRONMENT envar
                {
		  $$.func = accept_envar_e;
		  $$.into = $1;
		  $$.from = $envar;
                  ////  parser_accept_envar( *$1, *$envar );
                }
        |       accept_refer FROM COMMAND_LINE
                {
		  $$.func = accept_done_e;
                  parser_accept_command_line(*$1, NULL, NULL, NULL );
                }
        |       accept_refer FROM COMMAND_LINE '(' expr ')'
                {
		  $$.func = accept_command_line_e;
		  $$.into = $1;
		  $$.from = $expr;
                  //// parser_accept_command_line(*$1, $expr->field );
                }
        |       accept_refer FROM COMMAND_LINE_COUNT {
		  $$.func = accept_done_e;
                  parser_accept_command_line_count(*$1);
                }
                ;

accept_refer:   ACCEPT scalar { statement_begin(@1, ACCEPT); $$ = $2; }
                ;

accept_excepts:	accept_excepts[a] accept_except[b] statements %prec ACCEPT
                {
                  if( $a.on_error && $a.not_error ) {
                    error_msg(@b, "too many ON EXCEPTION clauses");
                    YYERROR;
                  }
                  // "ON" and "NOT ON" could be reversed, but not duplicated.
                  if( $a.on_error && $b.on_error ) {
                    error_msg(@b, "duplicate ON EXCEPTION clauses");
                    YYERROR;
                  }
                  if( $a.not_error && $b.not_error ) {
                    error_msg(@b, "duplicate NOT ON EXCEPTION clauses");
                    YYERROR;
                  }
                  $$ = $a;
                  if( $b.on_error ) {
                    $$.on_error = $b.on_error;
                    assert($a.not_error);
                  } else {
                    $$.not_error = $b.not_error;
                    assert($a.on_error);
                  }
                  assert( $b.on_error || $b.not_error );
                  assert( ! ($b.on_error && $b.not_error) );
                  cbl_label_t *tgt = $b.on_error? $b.on_error : $b.not_error;
                  parser_accept_exception_end(tgt);
                }
        |       accept_except[a] statements %prec ACCEPT
                {
                  $$ = $a;
                  assert( $a.on_error || $a.not_error );
                  assert( ! ($a.on_error && $a.not_error) );
                  cbl_label_t *tgt = $a.on_error? $a.on_error : $a.not_error;
                  parser_accept_exception_end(tgt);
                }
                ;

accept_except:	EXCEPTION
                {
                  $$.not_error = NULL;
                  $$.on_error = label_add(LblArith,
                                          uniq_label("accept"), yylineno);
                  if( !$$.on_error ) YYERROR;
                  parser_accept_exception( $$.on_error );

                  assert( $1 == EXCEPTION || $1 == NOT );
                  if( $1 == NOT ) {
                    std::swap($$.on_error, $$.not_error);
                  }
                }
		;

envar:          scalar { $$ = $1; $$->field->attr |= envar_e; }
        |       LITERAL {
                  $$ = new_reference(new_literal($1, quoted_e));
                  $$->field->attr |= envar_e;
                }
                ;

acceptable:     device_name
                {
                  $$ = symbol_special( $1.id );
                  if( !$$ ) {
                    error_msg(@1, "no such environment name");
                    YYERROR;
                  }
                }
        |       NAME
                {
                  $$ = special_of($1);
                  if( !$$ ) {
                    error_msg(@NAME, "no such environment mnemonic name: %s", $NAME);
                    YYERROR;
                  }
                }
                ;

add:            add_impl end_add { ast_add($1); }
        |       add_cond end_add { ast_add($1); }
                ;
add_impl:       ADD add_body
                {
                  statement_begin(@1, ADD);
                  $$ = $2;
                }
                ;
add_cond:       ADD add_body[body] arith_errs[err]
                {
                  statement_begin(@1, ADD);
                  $body->on_error = $err.on_error;
                  $body->not_error = $err.not_error;
                  $$ = $body;
                }
                ;
end_add:        %empty %prec ADD
        |       END_ADD
                ;

add_body:       sum TO rnames
                {
                  $$ = new arith_t(no_giving_e, $sum);
                  std::copy( rhs.begin(),
                             rhs.end(), back_inserter($$->tgts) );
                  rhs.clear();
                }
        |       sum TO num_operand[value] GIVING rnames
                {
                  $$ = new arith_t(giving_e, $sum);
                  $$->A.push_back(*$value);
                  std::copy( rhs.begin(),
                             rhs.end(), back_inserter($$->tgts) );
                  rhs.clear();
                }
        |       sum GIVING rnames
                { // implicit TO
                  $$ = new arith_t(giving_e, $sum);
                  std::copy( rhs.begin(),
                             rhs.end(), back_inserter($$->tgts) );
                  rhs.clear();
                }
        |       CORRESPONDING sum TO rnames
                {
                  corresponding_fields_t pairs =
                    corresponding_arith_fields( $sum->refers.front().field,
                                                rhs.front().refer.field );
                    if( pairs.empty() ) {
                      yywarn( "%s and %s have no corresponding fields",
                                $sum->refers.front().field->name,
                                rhs.front().refer.field->name );
                    }
                  // First src/tgt elements are templates.
                  // Their subscripts apply to the correspondents.
                  $$ = new arith_t(corresponding_e, $sum);
                  $$->tgts.push_front(rhs.front());
                  // use arith_t functor to populate A and tgts
                  *$$ = std::for_each( pairs.begin(), pairs.end(), *$$ );
                  $$->A.pop_front();
                  $$->tgts.pop_front();
                  rhs.clear();
                }
                ;

rounded:        %empty                  { $$ = truncation_e; }
        |       ROUNDED                 { $$ = current_rounded_mode(); }
        |       ROUNDED rounded_mode    { $$ = rounded_of($rounded_mode); }
                ;
rounded_mode:   MODE is rounded_type    { $$ = $rounded_type; }
                ;
rounded_type:   AWAY_FROM_ZERO          { $$ = away_from_zero_e; }
        |       NEAREST_TOWARD_ZERO     { $$ = nearest_toward_zero_e; }
        |       TOWARD_GREATER          { $$ = toward_greater_e; }
        |       TOWARD_LESSER           { $$ = toward_lesser_e; }
        |       round_between
                ;
round_between:  NEAREST_AWAY_FROM_ZERO  { $$ = nearest_away_from_zero_e; }
        |       NEAREST_EVEN            { $$ = nearest_even_e; }
        |       PROHIBITED              { $$ = prohibited_e; }
        |       TRUNCATION              { $$ = truncation_e; }
        ;

might_be:       %empty { $$ = IS; }
        |       MIGHT_BE
                ;

posneg:         POSITIVE { $$ = $1 == NOT? le_op : gt_op; }
        |       NEGATIVE { $$ = $1 == NOT? ge_op : lt_op; }
        |       ZERO     { $$ = $1 == NOT? ne_op : eq_op; }
                ;

scalar88s:	scalar88         { $$ = new refer_list_t($1); }
        |       scalar88s scalar88 { $1->push_back($2); }
                ;

name88:		NAME88 {
                  name_queue.qualify(@1, $1);
		  auto namelocs( name_queue.pop() );
		  auto names( name_queue.namelist_of(namelocs) );
                  if( ($$ = field_find(names)) == NULL ) {
                    if( procedure_div_e == current_division  ) {
		      error_msg(namelocs.back().loc,
				"DATA-ITEM '%s' not found", names.back() );
                      YYERROR;
                    }
		  }
		  assert($$->level == 88);
		}
		;

scalar88:	name88 subscripts[subs] refmod[ref]
                {
                  size_t n = $subs->size();
                  auto subscripts = new cbl_refer_t[n];
                  $subs->use_list(subscripts);
                  if( $ref.from->is_reference() || $ref.len->is_reference() ) {
                    error_msg(@subs, "subscripts on start:len refmod "
                            "parameters are unsupported");
                    YYERROR;
                  }
                  cbl_span_t span( $ref.from, $ref.len );
                  $$ = new cbl_refer_t($1, n, subscripts, span);
                }
        |       name88 refmod[ref]
                {
                  if( $ref.from->is_reference() || $ref.len->is_reference() ) {
                    error_msg(@ref, "subscripts on start:len refmod "
                            "parameters are unsupported");
                    YYERROR;
                  }
                  cbl_span_t span( $ref.from, $ref.len );
                  $$ = new cbl_refer_t($1, span);
                }
        |       name88 subscripts[subs]
                {
                  $$ = new cbl_refer_t($1);
                  if( $subs->refers.size() != $$->subscripts_set($subs->refers) ) {
                    subscript_dimension_error(@subs, $subs->refers.size(), $$);
                  }
                }
        |       name88
                {
                  $$ = new_reference($1);
                }
                ;

allocate:       ALLOCATE expr[size] CHARACTERS initialized RETURNING scalar[returning]
                {
                  statement_begin(@1, ALLOCATE);
                  if( $size->field->type == FldLiteralN ) {
		    auto size = TREE_REAL_CST_PTR ($size->field->data.value_of());
                    if( real_isneg(size) || real_iszero(size) ) { 
                      error_msg(@size, "size must be greater than 0");
                      YYERROR;
                    }
                  }
                  reject_refmod( @returning, *$returning );
                  if( ! require_pointer(@returning, *$returning) ) YYERROR;
                  parser_allocate( *$size, *$returning, $initialized );
                }
        |       ALLOCATE scalar[based] initialized alloc_ret[returning]
                {
                  statement_begin(@1, ALLOCATE);
                  if( ! $based->field->has_attr(based_e) ) {
                    error_msg(@based, "%s must be BASED", $based->name());
                    YYERROR;
                  }
                  reject_refmod( @based, *$based );
                  reject_refmod( @returning, *$returning );
                  if( $returning->field &&
		      ! require_pointer(@returning, *$returning) ) YYERROR;
                  parser_allocate( *$based, *$returning, $initialized );
                  if( $initialized ) {
                    initialize_allocated(*$based);
                  }
                }
                ;
initialized:    %empty       { $$ = false; }
        |       INITIALIZED  { $$ = true; }
                ;
alloc_ret:      %empty { static cbl_refer_t empty; $$ = &empty; }
        |       RETURNING scalar[name]           { $$ = $name; }
                ;

compute:        compute_impl end_compute { current.compute_end(); }
        |       compute_cond end_compute { current.compute_end(); }
                ;
compute_impl:   COMPUTE compute_body[body]
                {
                  parser_assign( $body.ntgt, $body.tgts, *$body.expr,
                                 NULL, NULL, current.compute_label() );
                  current.declaratives_evaluate(ec_none_e);
                }
                ;
compute_cond:   COMPUTE compute_body[body] arith_errs[err]
                {
                  parser_assign( $body.ntgt, $body.tgts, *$body.expr,
                                 $err.on_error, $err.not_error,
                                 current.compute_label() );
                  current.declaratives_evaluate(ec_size_e);
                }
                ;
end_compute:    %empty %prec COMPUTE
        |       END_COMPUTE
                ;

compute_body:   rnames { statement_begin(@$, COMPUTE); } compute_expr[expr] {
                  $$.ntgt = rhs.size();
                  auto C = new cbl_num_result_t[$$.ntgt];
                  $$.tgts = use_any(rhs, C);
                  $$.expr = $expr;
                }
                ;
compute_expr:   '=' {
                  current.compute_begin();
                } expr {
                  $$ = $expr;
                }
                ;
	|	EQUAL {
		  if( ! dialect_ibm() ) {
		    dialect_error(@1, "EQUAL invalid as assignment operator", "ibm");
		  }
		  current.compute_begin();
                } expr {
                  $$ = $expr;
                }
                ;

display:        disp_body end_display
                {
		  std::vector <cbl_refer_t> args($1.vargs->args.size());
		  std::copy( $1.vargs->args.begin(), $1.vargs->args.end(), args.begin() );
		  if( $1.special && $1.special->id == ARG_NUM_e ) {
		    if( $1.vargs->args.size() != 1 ) {
		      error_msg(@1, "ARGUMENT-NUMBER can be set to only one value");
		    }
		    cbl_refer_t& src( $1.vargs->args.front() );
		    cbl_field_t *dst = register_find("_ARGI");
		    parser_move( dst, src );
		  } else {
		    parser_display($1.special,
				   args.empty()? NULL : args.data(), args.size(),
				   DISPLAY_ADVANCE);
		  }
		  current.declaratives_evaluate(ec_none_e);
                }
        |       disp_body NO ADVANCING end_display
                {
                  std::vector <cbl_refer_t> args($1.vargs->args.size());
		  std::copy( $1.vargs->args.begin(), $1.vargs->args.end(), args.begin() );

		  if( $1.special && $1.special->id == ARG_NUM_e ) {
		    if( $1.vargs->args.size() != 1 ) {
		      error_msg(@1, "ARGUMENT-NUMBER can be set to only one value");
		    }
		    cbl_refer_t& src( $1.vargs->args.front() );
		    cbl_field_t *dst = register_find("_ARGI");
		    parser_move( dst, src );
		  } else {
		    parser_display($1.special,
				   args.empty()? NULL : args.data(), args.size(), 
				   DISPLAY_NO_ADVANCE);
		  }
		  current.declaratives_evaluate(ec_none_e);
                }
                ;
end_display:    %empty
        |       END_DISPLAY
                ;
disp_body:      disp_vargs[vargs]
                {
                  $$.special = NULL;
                  $$.vargs = $vargs;
                }
        |       disp_vargs[vargs] UPON disp_target[special]
                {
                  $$.special = $special;
                  $$.vargs = $vargs;
                }
                ;
disp_vargs:     DISPLAY vargs {
                  statement_begin(@1, DISPLAY);
                  $$ = $vargs;
                }
                ;

disp_target:    device_name {
                  $$ = symbol_special($1.id);
                }
        |       NAME
                {
                  symbol_elem_t *e = symbol_special(PROGRAM, $1);
                  if( !e ) {
                    error_msg(@NAME, "no such special name '%s'", $NAME);
                    YYERROR;
                  }
                  $$ = cbl_special_name_of(e);
                }
                ;

divide:         divide_impl end_divide { ast_divide($1); }
        |       divide_cond end_divide { ast_divide($1); }
                ;

divide_impl:    DIVIDE divide_body[body]
                {
                  statement_begin(@1, DIVIDE);
                  $$ = $body;
                }
                ;
divide_cond:    DIVIDE divide_body[body] arith_errs[err]
                {
                  statement_begin(@1, DIVIDE);
                  $$ = $body;
                  $$->on_error = $err.on_error;
                  $$->not_error = $err.not_error;
                }
                ;
end_divide:     %empty %prec DIVIDE
        |       END_DIVIDE
                ;

divide_body:    num_operand INTO rnames
                { /* format 1 */
                  $$ = new arith_t(no_giving_e);
                  $$->A.push_back(*$num_operand);
                  std::copy( rhs.begin(),
                             rhs.end(), back_inserter($$->tgts) );
                  rhs.clear();
                }
        |       divide_into
        |       divide_into REMAINDER scalar[rem]
                {
                  if( $1->tgts.size() != 1 ) {
                    error_msg(@1, "only 1 (not %zu) "
                             "GIVING with REMAINDER", $1->tgts.size());
                    YYERROR;
                  }
                  $$ = $1;
                  $$->remainder = *$rem;
                }
        |       divide_by
        |       divide_by   REMAINDER scalar[rem]
                {
                  if( $1->tgts.size() != 1 ) {
                    error_msg(@1, "only 1 (not %zu) "
                             "GIVING with REMAINDER", $1->tgts.size());
                    YYERROR;
                  }
                  $$ = $1;
                  $$->remainder = *$rem;
                }
                ;

divide_into:    num_operand[b] INTO num_operand[a] GIVING rnames
                { // format 2 & 4
                  $$ = new arith_t(giving_e);
                  $$->A.push_back(*$a);
                  $$->B.push_back(*$b);
                  std::copy( rhs.begin(),
                             rhs.end(), back_inserter($$->tgts) );
                  rhs.clear();
                }
                ;
divide_by:      num_operand[a] BY num_operand[b] GIVING rnames
                { // format 3 & 5
                  $$ = new arith_t(giving_e);
                  $$->A.push_back(*$a);
                  $$->B.push_back(*$b);
                  std::copy( rhs.begin(),
                             rhs.end(), back_inserter($$->tgts) );
                  rhs.clear();
                }
                ;

end_program:    end_program1[end] '.'
                {
                  const cbl_label_t *prog = current.program();
                  assert(prog);
                  const char *name = string_of($end.name);

                  bool matches = false;
                  const char *token_name = keyword_str($end.token) + 4;
                  switch($end.token) {
                  case END_PROGRAM:
                    matches = prog->type == LblProgram;
                    break;
                  case END_FUNCTION:
                    matches = prog->type == LblFunction;
                    break;
                  default:
                    error_msg(@end, "logic error: END token invalid '%s'", name);
                    gcc_unreachable();
                  }
                  if( !matches ) {
                    error_msg(@end, "END %s %s' does not match IDENTIFICATION DIVISION '%s'",
                              token_name, name, prog->name);
                    YYERROR;
                  }

                  if( 0 != strcasecmp(prog->name, name) ) {
                    error_msg(@end, "END PROGRAM '%s' does not match PROGRAM-ID '%s'",
                              name, prog->name);
                    YYERROR;
                  }
                  std::set<std::string> externals = current.end_program();
                  if( !externals.empty() ) {
		    for( auto name : externals ) {
		      yywarn("%s calls external symbol '%s'", prog->name, name.c_str());
		    }
                    YYERROR;
                  }
                  // pointer still valid because name is in symbol table
                  ast_end_program(prog->name);
                }
	|	end_program1[end] error
		{
		  const char *token_name = "???";
                  switch($end.token) {
                  case END_PROGRAM:
		    token_name = "PROGRAM";
                    break;
                  case END_FUNCTION:
		    token_name = "FUNCTION";
                    break;
                  default:
                    cbl_internal_error( "END token invalid");
                  }
		  error_msg(@end, "END %s requires NAME before '.'", token_name);
		  YYERROR;
		}
                ;
end_program1:   END_PROGRAM  namestr[name]
                {
                  $$.token = END_PROGRAM;
                  $$.name = $name;
                }
        |       END_FUNCTION namestr[name]
                {
                  $$.token = END_FUNCTION;
                  $$.name = $name;
                }
	|	END_PROGRAM  '.' // error
                {
                  $$.token = END_PROGRAM;
                }
        |       END_FUNCTION '.' // error
                {
                  $$.token = END_FUNCTION;
                }
                ;

continue_stmt:  CONTINUE {
                  statement_begin(@1, CONTINUE);
                  parser_sleep(*cbl_refer_t::empty());
                }
        |	CONTINUE AFTER expr SECONDS {
                  statement_begin(@1, CONTINUE);
                  parser_sleep(*$expr);
                }
                ;

exit:           GOBACK exit_with[status]
                {
		  statement_begin(@1, GOBACK);
		  parser_exit(*$status);
                }
	|	GOBACK exit_raising[ec]
		{
		  statement_begin(@1, GOBACK);
		  parser_exit(*cbl_refer_t::empty(), $ec);
		}
        |       EXIT { statement_begin(@1, EXIT); } exit_what
        |       SIMPLE_EXIT
                {
                  error_msg(@1, "EXIT is invalid here");
                }
                ;
		/* Valid "simple" EXIT (Format 1) swallowed by lexer */

		/*
		 * If the EXIT PROGRAM statement is executed in a program that
		 * is not under the control of a calling runtime element, the
		 * EXIT PROGRAM statement is treated as if it were a CONTINUE
		 * statement.
		 * To indicate this, We pass a "magic" refer with prog_func set.
		 */
exit_with:      %empty
                {
		  /* "If a RETURNING phrase is specified in the procedure
		   *  division header of the program containing the GOBACK
		   *  statement, the value in the data item referenced by that
		   *  RETURNING phrase becomes the result of the program
		   *  activation.  Execution continues in the calling element
		   *  as specified in the rules."
		   */
                  $$ = cbl_refer_t::empty();
		  if( dialect_ibm() ) {
		    static auto rt = cbl_field_of(symbol_at(return_code_register()));
		    static cbl_refer_t status(rt);
		    $$ = &status;
		  }
		  auto prog = cbl_label_of(symbol_at(current_program_index()));
		  if( prog->returning ) {
		    $$ = new cbl_refer_t( cbl_field_of(symbol_at(prog->returning)) );
		  }
                }
        |       with NORMAL stop_status
                {
                  $$ = $stop_status? $stop_status : new_reference(literally_zero);
                }
        |       with ERROR  stop_status
                {
                  $$ = $stop_status? $stop_status : new_reference(literally_one);
                }
	|	RETURNING stop_status
                {
		  if( ! dialect_mf() ) {
                    dialect_error(@2, "RETURNING <number>", "mf");
		  }
                  $$ = $stop_status? $stop_status : new_reference(literally_one);
                }
                ;
exit_what:      PROGRAM_kw                  { parser_exit_program(); }
        |       PROGRAM_kw exit_raising[ec] { parser_exit_program(); }
	|	SECTION			    { parser_exit_section(); }
	|	PARAGRAPH		    { parser_exit_paragraph(); }
        |       PERFORM   {
                  if( performs.empty() ) {
                    error_msg(@$, "EXIT PERFORM valid only "
                             "within inline PERFORM procedure" );
                    YYERROR;
                  }
                  parser_exit_perform(&perform_current()->tgt, $1);
                }
                ;

exit_raising:   RAISING EXCEPTION EXCEPTION_NAME[ec]
		{
		  $$ = $ec;
		}
	|	RAISING error {
		  cbl_unimplemented("RAISING exception-object");
		  $$ = ec_none_e;
		}
	|	RAISING LAST /* lexer swallows EXCEPTION */
		{
		  $$ = ec_all_e;
		}
		;

free:           FREE free_tgts
                {
                  size_t n = $free_tgts->size();
                  assert( n > 0 );
                  auto tgts = new cbl_refer_t[n];
                  parser_free( n, $free_tgts->use_list(tgts) );
                }
                ;
free_tgts:      free_tgt { $$ = new refer_list_t($1); }
        |       free_tgts free_tgt { $$->push_back($2); }
                ;
free_tgt:       scalar {
		  $$ = $1;
		  reject_refmod(@scalar, *$1);
		}
        |       ADDRESS OF scalar[name]
                {
                  $$ = $name;
                  $$->addr_of = true;
		  reject_refmod(@name, *$name);
                }
                ;

                /*
                 * Conditional Expressions
                 */
simple_cond:    kind_of_name
                {
                  $$ = new_reference($1);
                }
        |       SWITCH
                {
                  $$ = new_reference(new_temporary(FldConditional));
                  cbl_field_t *field = cbl_field_of(symbol_find(@1, $1));
                  assert(field->type == FldSwitch);
                  cbl_field_t *parent = parent_of(field);
                  size_t value = field->data.upsi_mask_of()->value;
                  bitop_t op = field->data.upsi_mask_of()->on_off?
                               bit_on_op : bit_off_op;
                   parser_bitop($$->cond(), parent, op, value );
                }
        |       expr is CLASS_NAME[domain]
                {
                  $$ = new_reference(new_temporary(FldConditional));
                  // symbol_find does not find FldClass symbols
                  struct symbol_elem_t *e = symbol_field(PROGRAM, 0, $domain);
                  parser_setop($$->cond(), $1->field, is_op, cbl_field_of(e));
                }
        |       expr NOT CLASS_NAME[domain] {
                  $$ = new_reference(new_temporary(FldConditional));
                  // symbol_find does not find FldClass symbols
                  struct symbol_elem_t *e = symbol_field(PROGRAM, 0, $domain);
                  parser_setop($$->cond(), $1->field, is_op, cbl_field_of(e));
                  parser_logop($$->cond(), NULL, not_op, $$->cond());
                }
        |       expr is OMITTED
                {
                  auto lhs = cbl_refer_t($expr->field);
                  lhs.addr_of = true;
                  auto rhs = cbl_field_of(symbol_field(0,0, "NULLS"));
                  $$ = new_reference(new_temporary(FldConditional));
                  parser_relop($$->field, lhs, eq_op, rhs);
                }
        |       expr NOT OMITTED
                {
                  auto lhs = cbl_refer_t($expr->field);
                  lhs.addr_of = true;
                  auto rhs = cbl_field_of(symbol_field(0,0, "NULLS"));
                  $$ = new_reference(new_temporary(FldConditional));
                  parser_relop($$->field, lhs, ne_op, rhs);
                }
        |       expr posneg[op] {
                  $$ = new_reference(new_temporary(FldConditional));
                  relop_t op = static_cast<relop_t>($op);
                  cbl_field_t *zero = constant_of(constant_index(ZERO));
                  parser_relop($$->cond(), *$1, op, zero);
                }
        |       scalar88 {
                  // copy the subscripts and set the parent field
                  cbl_refer_t parent = *$scalar88;
                  parent.field = parent_of($scalar88->field);
                  if( !parent.field ) {
                    cbl_internal_error("Type 88 has no referent");
                    YYERROR;
                  }
                  $$ = new_reference(new_temporary(FldConditional));
                  $$->field->parent = field_index($scalar88->field);
                  parser_relop($$->cond(), parent, eq_op, *$scalar88);
                }
                ;

kind_of_name:   expr might_be variable_type
                {
                  $$ = new_temporary(FldConditional);
                  enum classify_t type = classify_of($3);
                  assert(type != ClassInvalidType );

                  parser_classify( $$, *$1, type );
                  if( $2 == NOT ) {
                    parser_logop($$, NULL, not_op, $$);
                  }
                }
                ;

bool_expr:	log_expr { $$ = new_reference($1->resolve()); }
		;

log_expr:	log_term { $$ = new log_expr_t($1); }  %prec AND
	|	log_expr[lhs] OR rel_abbr[rhs]
		{
		  $$ = $1;
		  $$->or_term($rhs);
		}
	|	log_expr[lhs] OR log_expr[rhs]
                {
		  $$ = $lhs;
		  assert( ! $rhs->unresolved() ); // what to do?
		  $$->or_term($rhs->and_term());
		}
        |       log_expr[lhs] AND rel_abbr[rhs]
		{
		  $$ = $1;
		  $$->and_term($rhs);
		}
	|	log_expr[lhs] AND log_expr[rhs]
                {
		  $$ = $lhs;
		  assert( ! $rhs->unresolved() ); // what to do?
		  $$->and_term($rhs->and_term());
                }
		;

log_term:	'(' log_expr ')' {
		  current.antecedent_reset();
		  $$ = $log_expr->resolve();
		}
	|	NOT '(' log_expr ')' {
		  current.antecedent_reset();
		  $$ = $log_expr->resolve();
		  parser_logop($$, NULL, not_op, $$);
		}
	|	rel_expr
	|	simple_cond     {
		  current.antecedent_reset();
		  $$ = $1->cond();
		}
	|	NOT simple_cond {
		  current.antecedent_reset();
		  $$ = $2->cond();
		  parser_logop($$, NULL, not_op, $$);
		}
		;

rel_expr:	rel_lhs rel_term[rhs]
                {
		  rel_part_t& ante = current.antecedent();
		  if( $rhs.invert ) {
		    error_msg(@rhs, "NOT %s is invalid, cannot negate RHS",
			     ante.operand->field->name);
		  }
		  auto op = ante.relop;
		  if( ante.invert ) {
		    op = relop_invert(op);
		    ante.invert = false;
		  }
		  auto cond = new_temporary(FldConditional);
		  parser_relop( cond, *ante.operand, op, *$rhs.term );
		  $$ = cond;
                }
	|	rel_lhs[lhs] '(' rel_abbrs ')' {
		  $$ = $rel_abbrs->resolve();
		}
                ;

rel_abbrs:	rel_abbr { $$ = new log_expr_t($1); }
	|	'(' rel_abbrs ')' {
		  $$ = $2;
		  $$->resolve();

		}
	|	rel_abbrs OR rel_abbr[rhs] {
		  $$ = $1;
		  $$->or_term($rhs);
		}
	|	rel_abbrs OR '(' rel_abbr[rhs] ')' {
		  $$ = $1;
		  $$->or_term($rhs);
		}
	|	rel_abbrs AND rel_abbr[rhs] {
		  $$ = $1;
		  $$->and_term($rhs);
		}
	|	rel_abbrs AND '(' rel_abbr[rhs] ')' {
		  $$ = $1;
		  $$->and_term($rhs);
		}
                ;

rel_lhs:	rel_term[lhs] relop {
		  // no value, just set current antecedent
		  auto op   = relop_of($relop);
		  auto ante = new rel_part_t($lhs.term, op, $lhs.invert);
		  current.antecedent(*ante);
		}
		;

rel_abbr:	rel_term {
		  static rel_part_t ante;
		  ante = current.antecedent();
		  if( ! ante.operand ) {
		      error_msg(@1, "'AND %s' invalid because "
				"LHS is not a relation condition",
				name_of($rel_term.term->field) );
		    YYERROR;
		  }
		  assert(ante.has_relop);
		  if( $rel_term.invert ) ante.relop = relop_invert(ante.relop);
		  auto cond = new_temporary(FldConditional);
		  parser_relop(cond, *ante.operand, ante.relop, *$rel_term.term);
		  $$ = cond;
		}
	|	relop rel_term {
		  static rel_part_t ante;
		  if( $rel_term.invert ) {
		    error_msg(@2, "%s NOT %s is invalid",
			     keyword_str($relop),
		             name_of($rel_term.term->field));
		  }
		  auto op( relop_of($relop) );
		  ante = current.antecedent().relop_set(op);
		  if( ! ante.operand ) {
		      error_msg(@1, "AND %s invalid because "
				"LHS is not a relation condition",
				name_of($rel_term.term->field) );
		    YYERROR;
		  }
		  auto cond = new_temporary(FldConditional);
		  parser_relop(cond, *ante.operand, ante.relop, *$rel_term.term);
		  $$ = cond;
		}
		;

rel_term:	rel_term1
		;

rel_term1:	all LITERAL
                {
		  $$.invert = false;
                  $$.term = new_reference(new_literal($2, quoted_e));
                  $$.term->all = $all;
                }
        |       all spaces_etc[value]
                {
		  $$.invert = false;
                  $$.term = new_reference(constant_of(constant_index($value)));
                  $$.term->all = $all;
                }
        |       all NULLS
                {
		  $$.invert = false;
                  $$.term = new_reference(constant_of(constant_index(NULLS)));
                  $$.term->all = $all;
                }
        |       ALL ZERO
                { // ZERO without ALL comes from expr, from num_term.
		  $$.invert = false;
                  $$.term = new_reference(constant_of(constant_index(ZERO)));
                  $$.term->all = true;
                }
        |       expr {
		  $$.invert = false;
		  $$.term = $1;
		}
        |       NOT rel_term {
		  $$ = $2;
		  $$.invert = true;
		}
                ;

expr:           expr_term
                ;
expr_term:      expr_term '+' num_term
                {
                  if( ($$ = ast_op($1, '+', $3)) == NULL  ) YYERROR;
                }
        |       expr_term '-' num_term
                {
                  if( ($$ = ast_op($1, '-', $3)) == NULL  ) YYERROR;
                }
        |       num_term
                ;

num_term:       num_term '*' value
                {
                  if( ($$ = ast_op($1, '*', $3)) == NULL  ) YYERROR;
                }
        |       num_term '/' value
                {
                  if( ($$ = ast_op($1, '/', $3)) == NULL  ) YYERROR;
                }
        |       value
        ;

value:          value POW factor
                {
                  if( ($$ = ast_op($1, '^', $3)) == NULL  ) YYERROR;
                }
        |       '-' value       %prec NEG { $$ = negate( $2 );}
        |       '+' factor %prec NEG { $$ = $2;}
        |       factor[rhs]
                ;

factor:         '(' expr ')' { $$ = $2; }
        |       num_value { $$ = $num_value; }
                ;

if_stmt:        if_impl end_if
                ;

if_impl:        if_verb if_test if_body
                {
                  parser_fi();
                }
                ;
if_verb:        IF { statement_begin(@1, IF); }
                ;
if_test:        bool_expr then
                {
		  if( ! is_conditional($bool_expr) ) {
		    error_msg(@1, "%s is not a Boolean expression",
			     name_of($bool_expr->field) );
		    YYERROR;
		  }
                  parser_if( $bool_expr->cond() );
                }
                ;

if_body:        next_statements
                {
                  parser_else();
                }
        |       next_statements ELSE {
                  location_set(@2);
                  parser_else();
                } next_statements
                ;

next_statements: statements   %prec ADD
        |       NEXT SENTENCE %prec ADD
                {
                  next_sentence = label_add(LblNone, "next_sentence", 0);
                  parser_label_goto(next_sentence);
                }
                ;

end_if:         %empty %prec ADD
        |       END_IF
                ;

evaluate:       eval_verb eval_subjects eval_switch end_evaluate {
		  auto& ev( eval_stack.current() );
		  parser_label_label(ev.when());
		  parser_label_label(ev.done());
                  eval_stack.free();
		}
                ;
eval_verb:      EVALUATE {
                  statement_begin(@1, EVALUATE);
		  eval_stack.alloc();
                }
                ;

eval_subjects:  eval_subject
        |       eval_subjects ALSO eval_subject
                ;
eval_subject:   eval_subject1 {
		  auto& ev( eval_stack.current() );
		  ev.append(*$1);
		}
                ;
eval_subject1:  bool_expr
	|	expr
        |       true_false
                {
                  static cbl_field_t *zero = constant_of(constant_index(ZERO));
                  enum relop_t op = $1 == TRUE_kw? eq_op : ne_op;
                  $$ = new cbl_refer_t( new_temporary(FldConditional) );
                  parser_relop($$->field, zero, op, zero);
                }
                ;

eval_switch:    eval_cases
	|	eval_cases WHEN OTHER {
		  auto& ev( eval_stack.current() );
		  ev.write_when_label();
		}
		statements %prec ADD
                ;

eval_cases:                eval_case
        |       eval_cases eval_case
                ;

eval_case:      eval_objects statements %prec ADD {
		  auto& ev( eval_stack.current() );
		  parser_label_goto( ev.done() );
		  ev.rewind();
		}
        |       eval_objects NEXT SENTENCE %prec ADD
                {
		  auto& ev( eval_stack.current() );
		  ev.write_when_label();
                  next_sentence = label_add(LblNone, "next_sentence", 0);
                  parser_label_goto(next_sentence);
                }
                ;

eval_objects:	eval_whens {
		  auto& ev( eval_stack.current() );
		  // Place the object's Yeah label before the statements.
		  ev.write_yeah_label();
		}
		;
eval_whens:	eval_when
        |       eval_whens eval_when
                ;

eval_when:	WHEN {
		  auto& ev( eval_stack.current() );
		  ev.write_when_label();
		}
		eval_obj_cols %prec ADD { // all TRUE, go to statements
		  auto& ev( eval_stack.current() );
		  parser_label_goto(ev.yeah());
		  auto subj( ev.subject() );
		  if( subj ) {
		    error_msg(@2, "WHEN clause incomplete, %zu of %zu evaluated",
			     ev.object_count(), ev.subject_count());
		  }
		  ev.rewind();
		}
	|	WHEN error
                ;

eval_obj_cols:	eval_obj_col
        |       eval_obj_cols ALSO eval_obj_col
                ;

eval_obj_col:    ANY {
		  auto& ev( eval_stack.current() );
		  if( ! ev.decide(ANY) ) {
		    error_msg(@1, "WHEN 'ANY' phrase exceeds subject set count of %zu",
			     ev.subject_count());
		    YYERROR;
		  }
                }
        |       true_false {
		  auto& ev( eval_stack.current() );
		  auto subj( ev.subject() );
		  if( !subj ) {
		    error_msg(@$, "WHEN '%s' phrase exceeds subject set count of %zu",
			     keyword_str($1), ev.subject_count());
		    YYERROR;
		  }
		  if( ! is_conditional( subj ) ) {
		    error_msg(@1, "subject %s, type %s, "
			     "cannot be compared to TRUE/FALSE",
			     subj->name, 3 + cbl_field_type_str(subj->type) );
		  }
		  ev.decide($1);
                }
	|	eval_posneg[op] {
                  relop_t op = static_cast<relop_t>($op);
                  cbl_field_t *zero = constant_of(constant_index(ZERO));
		  auto& ev( eval_stack.current() );
		  auto subj( ev.subject() );
		  if( !subj ) {
		    error_msg(@1, "WHEN '%s' phrase exceeds subject set count of %zu",
			     relop_str(op), ev.subject_count());
		    YYERROR;
		  }
		  ev.decide(op, zero, false);
                }
	|	bool_expr {
		  auto& ev( eval_stack.current() );
		  auto subj( ev.subject() );
		  if( !subj ) {
		    error_msg(@1, "WHEN CONDITIONAL phrase exceeds "
			     "subject set count of %zu",
			     ev.subject_count());
		    YYERROR;
		  }
		  if( ! is_conditional( subj ) ) {
		    error_msg(@1, "subject %s, type %s, "
			      "cannot be compared to conditional expression",
			      subj->name, 3 + cbl_field_type_str(subj->type) );
		  }
		  ev.decide(*$1, false);
                }
         |       eval_abbrs {
		  auto& ev( eval_stack.current() );
		  ev.decided( $1->resolve() );
                }
	|	rel_term[a] THRU rel_term[b] %prec THRU {
		  auto& ev( eval_stack.current() );
		  auto subj( ev.subject() );
		  if( !subj ) {
		    error_msg(@a, "WHEN %s THRU %s phrase exceeds "
			     "subject set count of %zu",
			     $a.term->name(), $b.term->name(), ev.subject_count());
		    YYERROR;
		  }
		  if( is_conditional($a.term) || is_conditional($b.term) ) {
		    error_msg(@a, "THRU with boolean operand");
		  }
		  if( $b.invert ) {
		    error_msg(@b, "NOT %s is invalid with THRU",
			     name_of($b.term->field));
		  }
		  ev.decide(*$a.term, *$b.term, $a.invert);
                }
	|	rel_term[a] ELSE
		{
		  error_msg(@ELSE, "ELSE not valid in WHEN");
		  YYERROR;
		}
		;
eval_posneg:	POSITIVE { $$ = $1 == NOT? le_op : gt_op; }
        |       NEGATIVE { $$ = $1 == NOT? ge_op : lt_op; }
                ;

eval_abbrs:	rel_term[a] {
		  auto& ev( eval_stack.current() );
		  auto subj( ev.subject() );
		  if( !subj ) {
		    error_msg(@1, "WHEN %s phrase exceeds "
			     "subject set count of %zu",
			     $a.term->name(), ev.subject_count());
		    YYERROR;
		  }
		  if( ! ev.compatible($a.term->field) ) {
		    auto obj($a.term->field);
		    error_msg(@1, "subject %s, type %s, "
			     "cannot be compared %s, type %s",
			     subj->name, 3 + cbl_field_type_str(subj->type),
			     obj->name,	 3 + cbl_field_type_str(obj->type) );
		  }
		  auto result = ev.compare(*$a.term);
		  if( ! result ) YYERROR;
		  if( $a.invert ) {
		    parser_logop(result, nullptr, not_op, result);
		  }
		  $$ =  new log_expr_t(result);
                }
	|	relop rel_term[a] {
		  auto& ev( eval_stack.current() );
		  relop_t relop(relop_of($relop));
		  ev.object_relop(relop);
		  auto subj( ev.subject() );
		  if( !subj ) {
		    error_msg(@1, "WHEN %s %s phrase exceeds "
			     "subject set count of %zu",
			     relop_str(relop_of($relop)), $a.term->name(), ev.subject_count());
		    YYERROR;
		  }
		  if( ! ev.compatible($a.term->field) ) {
		    auto obj($a.term->field);
		    error_msg(@1, "subject %s, type %s, "
			     "cannot be compared %s, type %s",
			     subj->name, 3 + cbl_field_type_str(subj->type),
			     obj->name,	 3 + cbl_field_type_str(obj->type) );
		  }
		  if( is_conditional(ev.subject()) ) {
		    auto obj($a.term->field);
		    error_msg(@1, "subject %s, type %s, "
			     "cannot be %s %s, type %s",
			     subj->name, 3 + cbl_field_type_str(subj->type),
			      relop_str(relop_of($relop)),
			     obj->name,	 3 + cbl_field_type_str(obj->type) );
		  }
		  auto result = ev.compare(relop, *$a.term);
		  if( ! result ) YYERROR;
		  if( $a.invert ) {
		    parser_logop(result, nullptr, not_op, result);
		  }
		  $$ = new log_expr_t(result);
		}
	|	'(' eval_abbrs ')' {
		  $$ = $2;
		  $$->resolve();
		}
	|	eval_abbrs OR eval_abbr[rhs] {
		  $$ = $1;
		  $$->or_term($rhs);
		}
	|	eval_abbrs OR '(' eval_abbr[rhs] ')' {
		  $$ = $1;
		  $$->or_term($rhs);
		}
	|	eval_abbrs AND eval_abbr[rhs] {
		  $$ = $1;
		  $$->and_term($rhs);
		}
	|	eval_abbrs AND '(' eval_abbr[rhs] ')' {
		  $$ = $1;
		  $$->and_term($rhs);
		}
		;

eval_abbr:	rel_term[a] {
		  auto& ev( eval_stack.current() );
		  relop_t relop(ev.object_relop());
		  auto subj( ev.subject() );
		  assert( subj );
		  $$ = ev.compare(relop, *$a.term);
		  if( $a.invert ) {
		    parser_logop($$, nullptr, not_op, $$);
		  }
                }
	|	relop rel_term[a] {
		  auto& ev( eval_stack.current() );
		  relop_t relop(relop_of($relop));
		  ev.object_relop(relop);
		  $$ = ev.compare(relop, *$a.term);
		  if( $a.invert ) {
		    parser_logop($$, nullptr, not_op, $$);
		  }
                }
		;

end_evaluate:   %empty %prec EVALUATE
        |       END_EVALUATE
                ;

true_false:     TRUE_kw  { $$ = TRUE_kw; }
        |       FALSE_kw { $$ = FALSE_kw; }
                ;

scalar:         tableref {
		  // Check for missing subscript; others already checked.
                  if( $1->nsubscript == 0 && 0 < dimensions($1->field) ) {
                    subscript_dimension_error(@1, 0, $$);
                  }
		}
		;

tableref:	tableish {
		  // tableref is used by SORT.  It may name a table without subscripts.
		  $$ = $1;
		  $$->loc = @1;
		  if( $$->is_table_reference() ) {
                    if( $$->nsubscript != dimensions($$->field) ) {
                      subscript_dimension_error(@1, $$->nsubscript, $$);
		      YYERROR;
		    }
		  }
		}
tableish:	name subscripts[subs] refmod[ref]  %prec NAME
                {
		  assert(yychar != LPAREN);
                  $$ = new cbl_refer_t($name);
                  $$->subscripts_set($subs->refers);
                  literal_subscripts_valid( @subs, *$$ );
                  $$->refmod = cbl_span_t( $ref.from,
                                           $ref.len );
                  literal_refmod_valid( @ref, *$$ );
                }
        |       name refmod[ref]  %prec NAME
                {
                  $$ = new cbl_refer_t($name);
                  $$->refmod = cbl_span_t( $ref.from,
                                           $ref.len );
                  literal_refmod_valid( @ref, *$$ );
                }
        |       name subscripts[subs] %prec NAME
                {
                  $$ = new cbl_refer_t($name);
                  $$->subscripts_set($subs->refers);
                  literal_subscripts_valid( @subs, *$$ );
                }
        |       name %prec NAME
		{
		  $$ = new cbl_refer_t($name);
		}
                ;

refmod:         LPAREN expr[from] ':' expr[len] ')' %prec NAME
                {
		  if( ! require_numeric(@from, *$from) ) YYERROR;
		  if( ! require_numeric(@len, *$len) ) YYERROR;
                  $$.from = $from;
                  $$.len = $len;
                }
        |       LPAREN expr[from] ':'           ')' %prec NAME
                {
		  if( ! require_numeric(@from, *$from) ) YYERROR;
                  $$.from = $from;
                  $$.len = nullptr;
                }
                ;

typename:       NAME
                {
                  auto e = symbol_typedef(PROGRAM, $NAME);
                  if( ! e ) {
		    error_msg(@1, "DATA-ITEM '%s' not found", $NAME );
                    YYERROR;
                  }
                  $$ = cbl_field_of(e);
                }
                ;

name:           qname
                {
                  build_symbol_map();
		  auto namelocs( name_queue.pop() );
		  auto names( name_queue.namelist_of(namelocs) );
		  auto inner = namelocs.back();
                  if( ($$ = field_find(names)) == NULL ) {
                    if( procedure_div_e == current_division  ) {
		      error_msg(inner.loc,
				"DATA-ITEM '%s' not found", inner.name );
                      YYERROR;
                    }
                    /*
                     * Insert forward references, starting outermost.
                     */
                    size_t parent = 0;
                    while( ! names.empty() ) {
                      auto name = names.front();
                      names.pop_front();
                      auto e = symbol_field_forward_add(PROGRAM, parent,
                                                        name, yylineno);
                      if( !e ) YYERROR;
		      symbol_field_location( symbol_index(e), @qname );
                      parent = symbol_index(e);
                      $$ = cbl_field_of(e);
                    }
                  }
                  gcc_assert($$);
                }
                ;

qname:          ctx_name
                {
                  name_queue.qualify(@1, $1);
                }
        |       qname inof ctx_name
                {
                  name_queue.qualify(@3, $3);
                }
                ;
inof:           IN
        |       OF
                ;

ctx_name:       NAME
        |       context_word
                ;

context_word:   APPLY                   { static char s[] ="APPLY";
                                         $$ = s; } // screen description entry
        |       ARITHMETIC             { static char s[] ="ARITHMETIC";
                                         $$ = s; } // OPTIONS paragraph
        |       ATTRIBUTE              { static char s[] ="ATTRIBUTE";
                                         $$ = s; } // SET statement
        |       AUTO                   { static char s[] ="AUTO";
                                         $$ = s; } // screen description entry
        |       AUTOMATIC              { static char s[] ="AUTOMATIC";
                                         $$ = s; } // LOCK MODE clause
        |       AWAY_FROM_ZERO         { static char s[] ="AWAY-FROM-ZERO";
                                         $$ = s; } // ROUNDED phrase
        |       BACKGROUND_COLOR       { static char s[] ="BACKGROUND-COLOR";
                                         $$ = s; } // screen description entry
        |       BELL                   { static char s[] ="BELL";
                                         $$ = s; } // screen description entry and SET attribute statement
        |       BINARY_ENCODING        { static char s[] ="BINARY-ENCODING";
                                         $$ = s; } // USAGE clause and FLOAT-DECIMAL clause
        |       BLINK                  { static char s[] ="BLINK";
                                         $$ = s; } // screen description entry and SET attribute statement
        |       BYTE_LENGTH            { static char s[] ="BYTE-LENGTH";
                                         $$ = s; } // constant entry
        |       CAPACITY               { static char s[] ="CAPACITY";
                                         $$ = s; } // OCCURS clause
        |       CENTER                 { static char s[] ="CENTER";
                                         $$ = s; } // COLUMN clause
        |       CLASSIFICATION         { static char s[] ="CLASSIFICATION";
                                         $$ = s; } // OBJECT-COMPUTER paragraph
        |       CYCLE                  { static char s[] ="CYCLE";
                                         $$ = s; } // EXIT statement
        |       DECIMAL_ENCODING       { static char s[] ="DECIMAL-ENCODING";
                                         $$ = s; } // USAGE clause and FLOAT-DECIMAL clause
        |       EOL                    { static char s[] ="EOL";
                                         $$ = s; } // ERASE clause in a screen description entry
        |       EOS                    { static char s[] ="EOS";
                                         $$ = s; } // ERASE clause in a screen description entry
        |       ENTRY_CONVENTION       { static char s[] ="ENTRY-CONVENTION";
                                         $$ = s; } // OPTIONS paragraph
        |       ERASE                  { static char s[] ="ERASE";
                                         $$ = s; } // screen description entry
        |       EXPANDS                { static char s[] ="EXPANDS";
                                         $$ = s; } // class-specifier and interface-specifier of the REPOSITORY paragraph
        |       FEATURE                { static char s[] ="FEATURE";
                                         $$ = s; } // gcobol CDF token
        |       FLOAT_BINARY           { static char s[] ="FLOAT-BINARY";
                                         $$ = s; } // OPTIONS paragraph
        |       FLOAT_DECIMAL          { static char s[] ="FLOAT-DECIMAL";
                                         $$ = s; } // OPTIONS paragraph
        |       FOREGROUND_COLOR       { static char s[] ="FOREGROUND-COLOR";
                                         $$ = s; } // screen description entry
        |       FOREVER                { static char s[] ="FOREVER";
                                         $$ = s; } // RETRY phrase
        |       FULL                   { static char s[] ="FULL";
                                         $$ = s; } // screen description entry
        |       HIGH_ORDER_LEFT        { static char s[] ="HIGH-ORDER-LEFT";
                                         $$ = s; } // FLOAT-BINARY clause, FLOAT-DECIMAL clause, and USAGE clause
        |       HIGH_ORDER_RIGHT       { static char s[] ="HIGH-ORDER-RIGHT";
                                         $$ = s; } // FLOAT-BINARY clause, FLOAT-DECIMAL clause, and USAGE clause
        |       HIGHLIGHT              { static char s[] ="HIGHLIGHT";
                                         $$ = s; } // screen description entry and SET attribute statement
        |       IGNORING               { static char s[] ="IGNORING";
                                         $$ = s; } // READ statement
        |       IMPLEMENTS             { static char s[] ="IMPLEMENTS";
                                         $$ = s; } // FACTORY paragraph and OBJECT paragraph
        |       INITIALIZED            { static char s[] ="INITIALIZED";
                                         $$ = s; } // ALLOCATE statement and OCCURS clause
        |       INTERMEDIATE           { static char s[] ="INTERMEDIATE";
                                         $$ = s; } // OPTIONS paragraph
        |       INTRINSIC              { static char s[] ="INTRINSIC";
                                         $$ = s; } // function-specifier of the REPOSITORY paragraph
        |       LC_ALL_kw              { static char s[] ="LC_ALL";
                                         $$ = s; } // SET statement
        |       LC_COLLATE_kw          { static char s[] ="LC_COLLATE";
                                         $$ = s; } // SET statement
        |       LC_CTYPE_kw            { static char s[] ="LC_CTYPE";
                                         $$ = s; } // SET statement
        |       LC_MESSAGES_kw         { static char s[] ="LC_MESSAGES";
                                         $$ = s; } // SET statement
        |       LC_MONETARY_kw         { static char s[] ="LC_MONETARY";
                                         $$ = s; } // SET statement
        |       LC_NUMERIC_kw          { static char s[] ="LC_NUMERIC";
                                         $$ = s; } // SET statement
        |       LC_TIME_kw             { static char s[] ="LC_TIME";
                                         $$ = s; } // SET statement
        |       LOWLIGHT               { static char s[] ="LOWLIGHT";
                                         $$ = s; } // screen description entry and SET attribute statement
        |       MANUAL                 { static char s[] ="MANUAL";
                                         $$ = s; } // LOCK MODE clause
        |       MULTIPLE               { static char s[] ="MULTIPLE";
                                         $$ = s; } // LOCK ON phrase
        |       NEAREST_AWAY_FROM_ZERO { static char s[] ="NEAREST-AWAY-FROM-ZERO";
                                         $$ = s; } // INTERMEDIATE ROUNDING clause and ROUNDED phrase
        |       NEAREST_EVEN           { static char s[] ="NEAREST-EVEN";
                                         $$ = s; } // INTERMEDIATE ROUNDING clause and ROUNDED phrase
        |       NEAREST_TOWARD_ZERO    { static char s[] ="NEAREST-TOWARD-ZERO";
                                         $$ = s; } // INTERMEDIATE ROUNDING clause and ROUNDED phrase
        |       NONE                   { static char s[] ="NONE";
                                         $$ = s; } // DEFAULT clause
        |       NORMAL                 { static char s[] ="NORMAL";
                                         $$ = s; } // STOP statement
        |       NUMBERS                { static char s[] ="NUMBERS";
                                         $$ = s; } // COLUMN clause and LINE clause
        |       ONLY                   { static char s[] ="ONLY";
                                         $$ = s; } // Object-view, SHARING clause, SHARING phrase, and USAGE clause
        |       PREFIXED               { static char s[] ="PREFIXED";
                                         $$ = s; } // DYNAMIC LENGTH STRUCTURE clause
        |       PREVIOUS               { static char s[] ="PREVIOUS";
                                         $$ = s; } // READ statement
        |       PROHIBITED             { static char s[] ="PROHIBITED";
                                         $$ = s; } // INTERMEDIATE ROUNDING clause and ROUNDED phrase
        |       RECURSIVE              { static char s[] ="RECURSIVE";
                                         $$ = s; } // PROGRAM-ID paragraph
        |       RELATION               { static char s[] ="RELATION";
                                         $$ = s; } // VALIDATE-STATUS clause
        |       REQUIRED               { static char s[] ="REQUIRED";
                                         $$ = s; } // screen description entry
        |       REVERSE_VIDEO          { static char s[] ="REVERSE-VIDEO";
                                         $$ = s; } // screen description entry and SET attribute statement
        |       ROUNDING               { static char s[] ="ROUNDING";
                                         $$ = s; } // OPTIONS paragraph
        |       SECONDS                { static char s[] ="SECONDS";
                                         $$ = s; } // RETRY phrase
        |       SECURE                 { static char s[] ="SECURE";
                                         $$ = s; } // screen description entry
        |       SHORT                  { static char s[] ="SHORT";
                                         $$ = s; } // DYNAMIC LENGTH STRUCTURE clause
        |       SIGNED_kw              { static char s[] ="SIGNED";
                                         $$ = s; } // DYNAMIC LENGTH STRUCTURE clause and USAGE clause
        |       STANDARD_BINARY        { static char s[] ="STANDARD-BINARY";
                                         $$ = s; } // ARITHMETIC clause
        |       STANDARD_DECIMAL       { static char s[] ="STANDARD-DECIMAL";
                                         $$ = s; } // ARITHMETIC clause
        |       STATEMENT              { static char s[] ="STATEMENT";
                                         $$ = s; } // RESUME statement
        |       STEP                   { static char s[] ="STEP";
                                         $$ = s; } // OCCURS clause
        |       STRONG                 { static char s[] ="STRONG";
                                         $$ = s; } // TYPEDEF clause
        |       STRUCTURE              { static char s[] ="STRUCTURE";
                                         $$ = s; } // DYNAMIC LENGTH STRUCTURE clause
        |       SYMBOL                 { static char s[] ="SYMBOL";
                                         $$ = s; } // CURRENCY clause
        |       TOWARD_GREATER         { static char s[] ="TOWARD-GREATER";
                                         $$ = s; } // ROUNDED phrase
        |       TOWARD_LESSER          { static char s[] ="TOWARD-LESSER";
                                         $$ = s; } // ROUNDED phrase
        |       TRUNCATION             { static char s[] ="TRUNCATION";
                                         $$ = s; } // INTERMEDIATE ROUNDING clause and ROUNDED phrase
        |       UCS_4                  { static char s[] ="UCS-4";
                                         $$ = s; } // ALPHABET clause
        |       UNDERLINE              { static char s[] ="UNDERLINE";
                                         $$ = s; } // screen description entry and SET attribute statement
        |       UNSIGNED_kw            { static char s[] ="UNSIGNED";
                                         $$ = s; } // USAGE clause
        |       UTF_8                  { static char s[] ="UTF-8";
                                         $$ = s; } // ALPHABET clause
        |       UTF_16                 { static char s[] ="UTF-16";
                                         $$ = s; } // ALPHABET clause
        |       YYYYDDD                { static char s[] ="YYYYDDD";
                                         $$ = s; } // ACCEPT statement
        |       YYYYMMDD               { static char s[] ="YYYYMMDD";
                                         $$ = s; } // ACCEPT statement
                ;

move:           MOVE scalar TO move_tgts[tgts]
                {
                  statement_begin(@1, MOVE);
                  if( $scalar->field->type == FldIndex ) {
                    error_msg(@1, "'%s' cannot be MOVEd because it's an INDEX",
			     name_of($scalar->field) );
                    YYERROR;
                  }
                  if( !parser_move2($tgts, *$scalar) ) { YYERROR; }
                }
        |       MOVE all literalism[input] TO move_tgts[tgts]
                {
                  statement_begin(@1, MOVE);
                  struct cbl_refer_t *src = new_reference(new_literal($input,
                                                                      quoted_e));
                  src->all = $all;
                  if( !parser_move2($tgts, *src) ) { YYERROR; }
                }
        |       MOVE all spaces_etc[src] TO move_tgts[tgts]
                {
                  statement_begin(@1, MOVE);
                  cbl_field_t *field;
                  auto p = std::find_if( $tgts->targets.begin(),
                                         $tgts->targets.end(),
                                         [&field]( const auto& num_result ) {
                                             const cbl_refer_t& tgt = num_result.refer;
                                             field = tgt.field;
                                             return is_numeric(tgt.field);
                                            } );

                  if( p != $tgts->targets.end() ) {
                    error_msg(@src, "cannot MOVE %s "
			            "to numeric receiving field %s",
			      constant_of(constant_index($src))->name,
			      field->name );
                    YYERROR;
                  }

                  struct cbl_field_t* src = constant_of(constant_index($src));
                  if( !parser_move2($tgts, src) ) { YYERROR; }
                }
        |       MOVE all signed_literal[lit] TO move_tgts[tgts]
                {
                  statement_begin(@1, MOVE);
                  cbl_refer_t src( $lit, $all);
                  if( !parser_move2($tgts, src) ) { YYERROR; }
                }

        |       MOVE intrinsic_call TO move_tgts[tgts]
                {
                  statement_begin(@1, MOVE);
                  if( !parser_move2($tgts, *$2) ) { YYERROR; }
                }

        |       MOVE CORRESPONDING scalar[from] TO scalar[to]
                {
                  statement_begin(@1, MOVE);
                  if( $from->field->type != FldGroup ) {
                    error_msg(@from, "%s does not name a group", $from->name());
                    YYERROR;
                  }
                  if( $to->field->type != FldGroup ) {
                    error_msg(@to, "%s does not name a group", $to->name());
                    YYERROR;
                  }

                  if( !move_corresponding(*$to, *$from) ) {
                    yywarn( "%s and %s have no corresponding fields",
                            $from->field->name, $to->field->name );
                  }
                }
                ;

move_tgts:      move_tgt[tgt] {
                  $$ = new tgt_list_t;
                  if( $tgt ) list_add($$->targets, *$tgt, current_rounded_mode());
                }
        |       move_tgts move_tgt[tgt]
                {
                  if( $tgt ) list_add($1->targets, *$tgt, current_rounded_mode());
                }
		;
move_tgt:	scalar[tgt] {
		  if( is_literal($tgt->field) ) {
		    auto litcon = $tgt->field->name[0] == '_'? "literal" : "constant";
		    error_msg(@1, "%s is a %s", name_of($tgt->field), litcon);
		  }
		}
        |       literal {
		  const auto& field(*$1);
		  static char buf[32];
		  const char *value_str( name_of($literal) );
		  if( is_numeric($1) )
		  {
		    REAL_VALUE_TYPE val = TREE_REAL_CST (field.data.value_of());
		    int ival = (int)real_to_integer (&val);
		    val = real_value_truncate (TYPE_MODE (float_type_node),
					       val);
		    REAL_VALUE_TYPE rival;
		    real_from_integer (&rival, VOIDmode, ival, SIGNED);
		    if( real_identical (&val, &rival) ) {
		      sprintf(buf, "%d", ival);
		      value_str = buf;
		    }
		  }
		  auto litcon = field.name[0] == '_'? "literal" : "constant";
		  error_msg(@literal, "%s is a %s", value_str, litcon);
		  $$ = NULL;
		}
        |       error
                {
		  static const char * error_at;
		  if( error_at != yytext ) { // avoid repeated message
		    error_at = yytext;
		    error_msg(first_line_of(@1), "invalid receiving operand");
		  }
		  $$ = NULL;
                }
                ;

multiply:       multiply_impl end_multiply  { ast_multiply($1); }
        |       multiply_cond end_multiply  { ast_multiply($1); }
                ;
multiply_impl:  MULTIPLY multiply_body
                {
                  statement_begin(@1, MULTIPLY);
                  $$ = $2;
                }
                ;
multiply_cond:  MULTIPLY multiply_body[body] arith_errs[err]
                {
                  statement_begin(@1, MULTIPLY);
                  $$ = $body;
                  $$->on_error = $err.on_error;
                  $$->not_error = $err.not_error;
                }
                ;
end_multiply:   %empty %prec MULTIPLY
        |       END_MULTIPLY
                ;

multiply_body:  num_operand BY rnames
                {
                  $$ = new arith_t(no_giving_e);
                  $$->A.push_back(*$num_operand);
                  std::copy( rhs.begin(),
                             rhs.end(), back_inserter($$->tgts) );
                  rhs.clear();
                }
        |       num_operand BY signed_literal[lit]
                {
                  error_msg(@lit, "%s is not a receiving field", name_of($lit));
                  YYERROR;
                }
        |       num_operand[a] BY num_operand[b] GIVING rnames
                {
                  $$ = new arith_t(giving_e);
                  $$->A.push_back(*$a);
                  $$->B.push_back(*$b);
                  std::copy( rhs.begin(),
                             rhs.end(), back_inserter($$->tgts) );
                  rhs.clear();
                }
        |       num_operand[a] BY num_operand[b] GIVING signed_literal[lit]
                {
                  error_msg(@lit, "%s is not a receiving field", name_of($lit));
                  YYERROR;
                }
        |       LITERAL
                {
                  error_msg(@1, "invalid string operand '%s'", $1.data);
                  YYERROR;
                }
                ;

arith_errs:     arith_err[a] statements %prec ADD
                {
                  assert( $a.on_error || $a.not_error );
                  assert( ! ($a.on_error && $a.not_error) );
                  cbl_label_t *tgt = $a.on_error? $a.on_error : $a.not_error;
                  parser_arith_error_end(tgt);
                }
        |       arith_errs[a] arith_err[b] statements %prec ADD
                {
                  if( $a.on_error && $a.not_error ) {
                    error_msg(@1, "too many ON ERROR clauses");
                    YYERROR;
                  }
                  // "ON" and "NOT ON" could be reversed, but not duplicated.
                  if( $a.on_error && $b.on_error ) {
                    error_msg(@1, "duplicate ON ERROR clauses");
                    YYERROR;
                  }
                  if( $a.not_error && $b.not_error ) {
                    error_msg(@1, "duplicate NOT ON ERROR clauses");
                    YYERROR;
                  }
                  $$ = $a;
                  if( $b.on_error ) {
                    $$.on_error = $b.on_error;
                    assert($a.not_error);
                  } else {
                    $$.not_error = $b.not_error;
                    assert($a.on_error);
                  }
                  assert( $b.on_error || $b.not_error );
                  assert( ! ($b.on_error && $b.not_error) );
                  cbl_label_t *tgt = $b.on_error? $b.on_error : $b.not_error;
                  parser_arith_error_end(tgt);
                }
                ;

arith_err:      SIZE_ERROR
                {
                  assert( $1 == ERROR || $1 == NOT );
                  $$.on_error  = NULL;
                  $$.not_error = NULL;
                  cbl_label_t **ptgt = $1 == NOT? &$$.not_error : &$$.on_error;
                  if( current.in_compute() ) {
                    *ptgt = $1 == NOT?
                      current.compute_not_error() : current.compute_on_error();
                  } else {
                    *ptgt = label_add(LblArith, uniq_label("arith"), yylineno);
                  }
		  (*ptgt)->lain = yylineno;
                  parser_arith_error( *ptgt );
                }
                ;

   /*
    * Relational operator         Can be written
    * IS GREATER THAN             IS >
    * IS NOT GREATER THAN         IS NOT >
    * IS LESS THAN                IS <
    * IS NOT LESS THAN            IS NOT <
    * IS EQUAL TO                 IS =
    * IS NOT EQUAL TO             IS NOT =
    * IS GREATER THAN OR EQUAL TO IS >=
    * IS LESS THAN OR EQUAL TO    IS <=
    *
    * The lexer returns simple tokens.
    */

relop:          '<' { $$ = '<'; }
        |       LE  { $$ = LE;  }
        |       '=' { $$ = '='; }
        |       NE  { $$ = NE;  }
        |       GE  { $$ = GE;  }
        |       '>' { $$ = '>'; }
                ;

rnames:         scalar rounded
                {
                  list_add( rhs, *$scalar, $rounded );
                }
        |       rnames scalar rounded
                {
                  cbl_num_result_t arg = { static_cast<cbl_round_t>($rounded),
                                           *$scalar };
                  rhs.push_back(arg);
                }
                ;

sum:                num_operand     { $$ = new refer_list_t($num_operand); }
        |       sum num_operand     { $$->push_back($num_operand); }
                ;

num_operand:    scalar
        |       signed_literal { $$ = new_reference($1); }
        |       intrinsic_call
                ;

num_value:      scalar // might actually be a string
        |       intrinsic_call
        |       num_literal { $$ = new_reference($1); }
        |       ADDRESS OF scalar {$$ = $scalar; $$->addr_of = true; }
        |       DETAIL OF scalar {$$ = $scalar; }
        |       LENGTH_OF name[val] {
                  location_set(@1);
                  $$ = new cbl_refer_t( new_tempnumeric() );
		  $$->field->clear_attr(signable_e);
                  if( dialect_gcc() ) {
		    dialect_error(@1, "LENGTH OF", "ibm");
                  }
		  parser_set_numeric($$->field, $val->data.capacity);
                }
        |       LENGTH_OF name[val] subscripts[subs] {
                  location_set(@1);
                  $$ = new cbl_refer_t( new_tempnumeric() );
		  $$->field->clear_attr(signable_e);
                  if( dialect_gcc() ) {
		    dialect_error(@1, "LENGTH OF", "ibm");
                  }
                  if( 0 == dimensions($val) ) {
		    cbl_refer_t r1($val);
		    subscript_dimension_error( @subs, $subs->refers.size(), &r1 );
                  }
		  parser_set_numeric($$->field, $val->data.capacity);
                }
                ;


                /*
                 * Constant Compile-time Expressions
                 */

/* cce_cond_expr:  cce_bool_expr { $$ = $1 == 0? false : true; } */
/*              ; */
/* cce_bool_expr:       cce_and */
/*      |       cce_bool_expr OR cce_and  { $$ = $1 || $3; } */
/*      ; */
/* cce_and:     cce_reloper */
/*      |       cce_and AND cce_reloper { $$ = $1 && $3; } */
/*              ; */
/* cce_reloper:     cce_relexpr */
/*      |       NOT cce_relexpr { $$ = $2 != 0; } */
/*              ; */
/* cce_relexpr: cce_expr */
/*      |       cce_relexpr '<' cce_expr { $$ = $1 <  $3; } */
/*      |       cce_relexpr LE  cce_expr { $$ = $1 <= $3; } */
/*      |       cce_relexpr '=' cce_expr { $$ = $1 == $3; } */
/*      |       cce_relexpr NE  cce_expr { $$ = $1 != $3; } */
/*      |       cce_relexpr GE  cce_expr { $$ = $1 >= $3; } */
/*      |       cce_relexpr '>' cce_expr { $$ = $1 >  $3; } */
/*              ; */

cce_expr:       cce_factor
        |       cce_expr '+' cce_expr {
                  real_arithmetic (&$$, PLUS_EXPR, &$1, &$3);
                  real_convert (&$$, TYPE_MODE (float128_type_node), &$$);
                }
        |       cce_expr '-' cce_expr {
                  real_arithmetic (&$$, MINUS_EXPR, &$1, &$3);
                  real_convert (&$$, TYPE_MODE (float128_type_node), &$$);
                }
        |       cce_expr '*' cce_expr {
                  real_arithmetic (&$$, MULT_EXPR, &$1, &$3);
                  real_convert (&$$, TYPE_MODE (float128_type_node), &$$);
                }
        |       cce_expr '/' cce_expr {
                  real_arithmetic (&$$, RDIV_EXPR, &$1, &$3);
                  real_convert (&$$, TYPE_MODE (float128_type_node), &$$);
                }
        |                '+' cce_expr %prec NEG { $$ =  $2; }
        |                '-' cce_expr %prec NEG { $$ = real_value_negate (&$2); }
        |                '(' cce_expr ')'  { $$ = $2; }
        ;

cce_factor:     NUMSTR {
                  /* real_from_string does not allow arbitrary radix.  */
                  // When DECIMAL IS COMMA, commas act as decimal points.
		  gcc_assert($1.radix == decimal_e);
		  auto p = $1.string, pend = p + strlen(p);
		  std::replace(p, pend, ',', '.');
		  real_from_string3( &$$, $1.string,
				     TYPE_MODE (float128_type_node) );
                }
                ;

                /*
                 * End Constant Compile-time Expressions
                 */

section_name:	NAME section_kw '.'
                {
                  statement_begin(@1, SECTION);
		  $$ = label_add(@1, LblSection, $1);
                  ast_enter_section($$);
                  apply_declaratives();
                }
	|	NAME section_kw // lexer swallows '.' before USE
                <label>{
                  statement_begin(@1, SECTION);
		  $$ = label_add(@1, LblSection, $1);
                  ast_enter_section($$);
                  apply_declaratives();
                } [label]
                cdf_use dot
                {
                  $$ = $label;
                }
                ;

section_kw:     SECTION
                {
                  if( $1 ) {
		    if( *$1 == '-' ) {
		      error_msg(@1, "SECTION segment %s is negative", $1);
                    } else {
                      cbl_unimplementedw("SECTION segment %s was ignored", $1);
                    }
		  }
                }
        |       SECTION error
                {
                  error_msg(@1, "unknown section qualifier");
                }
                ;

stop:           STOP RUN exit_with
                {
                  statement_begin(@1, STOP);
                  parser_see_stop_run( *$exit_with, NULL );
                }
        |       STOP NUMSTR[status] // IBM syntax
                {
                  statement_begin(@1, STOP);
                  if( ! dialect_ibm() ) {
                    dialect_error(@2, "STOP <number> is not ISO syntax,", "ibm");
                    YYERROR;
                  }
                  cbl_refer_t status( new_literal($status.string, $status.radix) );
                  parser_see_stop_run( status, NULL );
                }
        |       STOP LITERAL[name] // CCVS-85 && IBM syntax
                {
                  statement_begin(@1, STOP);
                  const char *name = string_of($name);
                  if( ! name ) {
                    error_msg(@name, "'%s' has embedded NUL", $name.data);
                    YYERROR;
                  }
                  parser_see_stop_run( literally_zero, $name.data );
                }
                ;
stop_status:    status         { $$ = NULL; }
        |       status scalar  { $$ = $2; }
        |       status NUMSTR {
                  $$ = new_reference(new_literal($2.string, $2.radix));
                }
                ;

subscripts:     LPAREN expr_list ')' {
		  $$ = $2;
		  const auto& exprs( $$->refers );
		  bool ok = std::all_of( exprs.begin(), exprs.end(),
					 []( const auto& refer ) {
					     return is_numeric(refer.field);
					 } );
		  if( ! ok ) {
		    int i=0;
		    for( auto refer : exprs ) {
		      if( ! is_numeric(refer.field) ) {
			  error_msg(@1, "subscript %d, %s, is not numeric (%s)",
				   ++i, name_of(refer.field),
				   cbl_field_type_str(refer.field->type) + 3);
		      }
		    }
		    YYERROR;
		  }
		}
                ;
expr_list:	expr
		{
		  if( ! require_numeric(@expr, *$expr) ) YYERROR;
		  $$ = new refer_list_t($expr);
		}
        |       expr_list expr {
                  if( $1->size() == MAXIMUM_TABLE_DIMENSIONS ) {
                    error_msg(@1, "table dimensions limited to %d",
                             MAXIMUM_TABLE_DIMENSIONS);
                    YYERROR;
                  }
		  if( ! require_numeric(@expr, *$expr) ) YYERROR;
                  $1->push_back($2); $$ = $1;
                }
        |       ALL {
                  auto ref = new_reference(constant_of(constant_index(ZERO)));
                  $$ = new refer_list_t(ref);
                }
                ;

arg_list:                any_arg { $$ = new refer_list_t($1); }
        |       arg_list any_arg { $1->push_back($2); $$ = $1; }
                ;
any_arg:        expr
        |       LITERAL {$$ = new_reference(new_literal($1, quoted_e)); }
                ;

                /*
                 * Because num_literal includes ZERO, this grammar
                 * allows -ZERO and +ZERO. FWIW.
                 */
signed_literal:     num_literal
        |       '+' num_literal { $$ = $2; }
        |       '-' num_literal
                {
                  $$ = new_tempnumeric();
                  struct cbl_field_t *zero = constant_of(constant_index(ZERO));
                  parser_subtract( $$, zero, $2, current_rounded_mode() );
                }
        |       LENGTH_OF name[val] {
                  location_set(@1);
                  $$ = new_tempnumeric();
		  $$->clear_attr(signable_e);
                  if( dialect_gcc() ) {
		    dialect_error(@1, "LENGTH OF", "ibm");
                  }
                  parser_set_numeric($$, $val->data.capacity);
                }
        |       LENGTH_OF name[val] subscripts[subs] {
                  location_set(@1);
                  $$ = new_tempnumeric();
		  $$->clear_attr(signable_e);
                  if( dialect_gcc() ) {
		    dialect_error(@1, "LENGTH OF", "ibm");
                  }
                  if( 0 == dimensions($val) ) {
		    cbl_refer_t r1($val);
		    subscript_dimension_error( @subs, $subs->refers.size(), &r1 );
                  }
                  parser_set_numeric($$, $val->data.capacity);
                }
                ;

num_literal:    NUMSTR { $$ = new_literal($1.string, $1.radix); }
        |       ZERO   { $$ = constant_of(constant_index(ZERO)); }
                ;

open:           OPEN { statement_begin(@1, OPEN); } open_files
                ;
open_files:     open_file
        |       open_files open_file
                ;
open_file:      open_io[mode] filenames {
                  size_t n = $2->files.size();
                  parser_file_open( n, use_list($2->files, false), $mode );
                  current.declaratives_evaluate($2->files);
                  $2->files.clear();
                }
                ;
open_io:        INPUT  { $$ = 'r'; }
        |       OUTPUT { $$ = 'w'; }
        |       EXTEND { $$ = 'a'; }
        |       IO     { $$ = '+'; }
        ;

close:          CLOSE { statement_begin(@1, CLOSE); } close_files
                ;
close_files:    close_file
        |       close_files close_file
                ;
close_file:     NAME close_how
                {
                  struct symbol_elem_t *e = symbol_file(PROGRAM, $1);
                  if( !e ) {
                    error_msg(@1, "invalid file name '%s'", $1);
                    YYERROR;
                  }
                  auto how = static_cast<file_close_how_t>($close_how);
                  bool reel_unit = (file_close_reel_unit_e & $close_how) > 0;
                  auto file = cbl_file_of(e);
                  switch( file->org ) {
                  case file_disorganized_e:
                    gcc_unreachable();
                    break;
                  case file_sequential_e:
                  case file_line_sequential_e:
                    break;
                  case file_indexed_e:;
                  case file_relative_e:
                    if( $close_how & ~file_close_with_lock_e ) {
                      error_msg(@1, "INDEXED or RELATIVE file "
                               "closed with incompatible qualifier" );
                      YYERROR;
                    }
                    break;
                  }
                  if(reel_unit)
                    {
                    how = file_close_reel_unit_e;
                    }
                  parser_file_close( file, how );
                  current.declaratives_evaluate( file );
                }
                ;
close_how:      %empty                   { $$ = file_close_no_how_e; }
        |       reel_unit                { $$ = file_close_reel_unit_e; }
        |       reel_unit for_kw REMOVAL {
                  $$ = file_close_reel_unit_e | file_close_removal_e;
                }
        |       reel_unit WITH NO REWIND {
                  $$ = file_close_reel_unit_e | file_close_no_rewind_e;
                }
        |       with NO REWIND           { $$ = file_close_no_rewind_e; }
        |       with LOCK                { $$ = file_close_with_lock_e; }
        ;
reel_unit:      REEL
        |       UNIT
                ;
for_kw:         %empty
        |       FOR
                ;

perform:        perform_verb perform_proc   { perform_free(); }
        |       perform_verb perform_stmts  {
		  perform_ec_cleanup();
		  perform_free();
		}
        |       perform_verb perform_except {
		  perform_ec_cleanup();
		  perform_free();
		}
                ;

perform_stmts:  perform_until perform_inline[in]
                {
                  std::vector <cbl_perform_vary_t> varys($in->varys.size());
                  std::copy( $in->varys.begin(), $in->varys.end(), varys.begin() );

                  parser_perform_until(&$in->tgt, $in->before,
				       varys.size(), varys.data());
                }
        |       perform_vary perform_inline[in]
                {
		  struct perform_t *p = $in;
                  std::vector <cbl_perform_vary_t> varys(p->varys.size());
                  std::copy( p->varys.begin(), p->varys.end(), varys.begin() );

                  parser_perform_until(&$in->tgt, $in->before,
				       varys.size(), varys.data());
                }
        |       perform_times perform_inline[in]
                {
                  parser_perform_inline_times(&$in->tgt, *$perform_times);
                }
        |       perform_inline[in]
                {
                  parser_perform_inline_times(&$in->tgt, literally_one);
                }
                ;

perform_proc:   perform_names %prec NAME
                {
                  struct perform_t *p = perform_current();
                  if( yydebug ) p->tgt.dump();
                  parser_perform(&p->tgt, NULL);
                }
        |       perform_names num_operand TIMES
                {
                  struct perform_t *p = perform_current();
                  if( yydebug ) p->tgt.dump();
                  parser_perform(&p->tgt, *$2);
                }
        |       perform_names perform_until
                {
                  struct perform_t *p = perform_current();
                  if( yydebug ) p->tgt.dump();
                  assert(1 == p->varys.size());
                  parser_perform_until( &p->tgt, p->before, 1, &p->varys.front() );
                }
        |       perform_names perform_vary
                {
                  struct perform_t *p = perform_current();
                  if( yydebug ) p->tgt.dump();

                  std::vector <cbl_perform_vary_t> varys(p->varys.size());
                  std::copy( p->varys.begin(), p->varys.end(), varys.begin() );

                  parser_perform_until( &p->tgt, p->before, varys.size(), varys.data() );
                }
                ;

perform_names:  label_1[para]
                {
                  perform_tgt_set($para);
                }
        |       label_1[para1] THRU label_1[para2]
                {
                  perform_tgt_set($para1, $para2);
                }
                ;

perform_times:  num_operand TIMES
                {
                  $$ = $1;
                }
                ;

perform_vary:   test_before varying vary_afters
                {
                  perform_current()->before = $1 == BEFORE;
                }
        |                   varying vary_afters
        |       test_before varying
                {
                  perform_current()->before = $1 == BEFORE;
                }
        |              varying
                ;

perform_verb:   PERFORM {
                  statement_begin(@1, PERFORM);
                  $$ = perform_alloc();
                }
                ;

perform_until:  test_before perform_cond
                {
                  struct perform_t *p = perform_current();
                  struct cbl_perform_vary_t vary;

                  p->before = $1 == BEFORE;
                  vary.until = $2;
                  p->varys.push_back(vary);
                }
        |       perform_cond
                {
                  struct perform_t *p = perform_current();
                  struct cbl_perform_vary_t vary;

                  vary.until = $1;
                  p->varys.push_back(vary);
                }
                ;
perform_cond:   UNTIL { parser_perform_conditional( &perform_current()->tgt); }
                bool_expr
                {
                  parser_perform_conditional_end( &perform_current()->tgt);
		  if( !is_conditional($bool_expr) ) {
		    error_msg(@1, "%s is not a condition expression",
		             name_of($bool_expr->field));
		    YYERROR;
		  }
                  $$ = $bool_expr->cond();
                }
                ;

perform_inline: perform_start statements END_PERFORM
                {
		  location_set(@END_PERFORM);
		  $$ = perform_current();
		  if( $perform_start == LOCATION ) {
		    error_msg(@1, "LOCATION not valid with PERFORM Format 2");
		  }
                }
        |       perform_start END_PERFORM
                {
		  location_set(@END_PERFORM);
		  $$ = perform_current();
		  if( $perform_start == LOCATION ) {
		    error_msg(@1, "LOCATION not valid with PERFORM Format 2");
		  }
                }
                ;
perform_start:	%empty %prec LOCATION {
		  perform_ec_setup();
		  $$ = 0;
		}
	|	with LOCATION {
		  perform_ec_setup();
		  $$ = LOCATION;
		}
		;

perform_except:	perform_start
		statements
		{
		  auto perf = perform_current();
                  parser_perform_inline_times(&perf->tgt, literally_one);
		}
		perform_when      // paragraphs
		perform_ec_other  // paragraph
		perform_ec_common // paragraph
		{
		  auto perf = perform_current();
		  parser_label_goto(perf->ec_labels.finally);
		}
		perform_ec_finally
		END_PERFORM
                {
		  auto perf = perform_current();
		  // produce blob, jumped over by FINALLY paragraph
		  size_t iblob = symbol_declaratives_add( PROGRAM, perf->dcls );
		  auto lave = perf->ec_labels.new_label(LblParagraph, "lave");
		  auto handlers = cbl_field_of(symbol_at(iblob));

		  // install blob
		  parser_label_label(perf->ec_labels.init);
		  declarative_runtime_match(handlers, lave);

		  // uninstall blob
		  parser_label_label(perf->ec_labels.fini);
                }
		;

perform_when:	perform_when1
	|	perform_when perform_when1
		;
perform_when1:	WHEN perform_ec {
		// accumulate handlers and their paragraphs
		  auto perf = perform_current();
		  auto when = perf->ec_labels.new_label(LblParagraph, "when");
		  for( auto& dcl : $perform_ec->elems ) {
		    // use section to hold paragraph
		    dcl->section = symbol_index(symbol_elem_of(when));
		  }
		  std::transform( $perform_ec->elems.begin(),
				  $perform_ec->elems.end(),
				  std::back_inserter(perf->dcls),
				  []( cbl_declarative_t *p ) {
				    return *p;
				  } );
		  ast_enter_paragraph(when);
		}
		statements {
		  parser_exit_paragraph();
		}
		;

perform_ec:	EXCEPTION filenames {
		  auto dcls = new declarative_list_t;
		  auto p = $filenames->files.begin();
		  auto pend = p;
		  while( pend != $filenames->files.end() ) {
		    for( size_t i=0; i < COUNT_OF(cbl_declarative_t::files); i++ ) {
		      if( ++pend == $filenames->files.end() ) break;
		    }
		    std::list<size_t> files;
		    std::transform( p, pend, std::back_inserter(files),
		                  []( const cbl_file_t* f ) {
		                      return symbol_index(symbol_elem_of(f)); } );

		    auto dcl = new cbl_declarative_t(0, ec_io_e, files, file_mode_none_e);
		    dcls->elems.push_back(dcl);
		  }
		  $$ = dcls;
		}
	|	EXCEPTION io_mode {
		  auto dcl = new cbl_declarative_t($io_mode);
		  $$ = new declarative_list_t(dcl);
		}
	|	except_names {
		  auto dcls = new declarative_list_t;
		  const ec_list_t * ecs($except_names);
		  // one cbl_declarative_t per EC
		  std::transform( ecs->elems.begin(), ecs->elems.end(),
				  std::back_inserter(dcls->elems),
				  []( ec_type_t ec )
				  {
				    return new cbl_declarative_t(ec);
				  } );
		  $$ = dcls;
		}
	|	except_files {
		  // one cbl_declarative_t per 16 files
		  auto dcls = new declarative_list_t;
		  for( auto p = $except_files->elems.begin();
		       p != $except_files->elems.end(); ) {
		    auto dcl = new cbl_declarative_t;
		    for( auto file = dcl->files;
			 file < dcl->files + COUNT_OF(dcl->files); file++ ) {
		      if( p != $except_files->elems.end() ) break;
		      *file = *p++;
		    }
		    dcls->elems.push_back(dcl);
		  }
		  $$ = dcls;
		}
		;

except_names:	except_name { $$ = new ec_list_t($1); }
	|	except_names except_name {
		  $$ = $1->push_back($2);
		}
		;
except_name:	EXCEPTION_NAME[ec] {
		  assert($ec != ec_none_e);
		  $$ = $1;
		}
		;

except_files:	except_name[ec] FILE_KW filenames {
		  assert($ec != ec_none_e);
		  if( ec_io_e != (ec_io_e & $ec) ) {
		    error_msg(@1, "%s is not of type EC-I-O",
			     ec_type_str($ec));
		  }
		  $$ = new isym_list_t;
		  std::list<size_t>& files( $$->elems );
		  std::transform( $filenames->files.begin(),
				  $filenames->files.end(),
		                  std::back_inserter(files),
		                  []( const cbl_file_t* f ) {
		                      return symbol_index(symbol_elem_of(f)); } );
		}
		;

perform_ec_other:
		%empty %prec WHEN {
		  auto& ec_labels( perform_current()->ec_labels );
		  ast_enter_paragraph(ec_labels.other);
		  parser_exit_paragraph();
		}
	|	WHEN OTHER {
		  auto& ec_labels( perform_current()->ec_labels );
		  ast_enter_paragraph(ec_labels.other);
		}
		exception statements %prec WHEN {
		  parser_exit_paragraph();
		}
		;
perform_ec_common:
		%empty {
		  auto& ec_labels( perform_current()->ec_labels );
		  ast_enter_paragraph(ec_labels.common);
		  parser_exit_paragraph();
		}
	|	WHEN COMMON {
		  auto& ec_labels( perform_current()->ec_labels );
		  ast_enter_paragraph(ec_labels.common);
		}
		exception statements {
		  parser_exit_paragraph();
		}
		;
perform_ec_finally:
		%empty {
		  auto& ec_labels( perform_current()->ec_labels );
		  ast_enter_paragraph(ec_labels.finally);
		  parser_exit_paragraph();
		  parser_label_goto(ec_labels.fini);
		}
	|	FINALLY {
		  auto& ec_labels( perform_current()->ec_labels );
		  ast_enter_paragraph(ec_labels.finally);
		}
		exception statements {
		  parser_exit_paragraph();
		  auto& ec_labels( perform_current()->ec_labels );
		  parser_label_goto(ec_labels.fini);
		}
		;

test_before:    with TEST BEFORE { $$ = BEFORE; }
        |       with TEST AFTER  { $$ = AFTER; }
                ;

varying:        VARYING num_operand[tgt] FROM num_operand[from] vary_by[by]
                        perform_cond[until]
                {
                  struct cbl_perform_vary_t vary(*$tgt, *$from, *$by, $until);
                  perform_current()->varys.push_back(vary);
                }
                ;

vary_afters:    vary_after
        |       vary_afters vary_after
                ;
vary_after:     AFTER num_operand[tgt] FROM num_operand[from] vary_by[by]
                      perform_cond[until]
                {
                  struct cbl_perform_vary_t vary(*$tgt, *$from, *$by, $until);
                  perform_current()->varys.push_back(vary);
                }
                ;
vary_by:        %empty         { $$ = new cbl_refer_t(literally_one); }
        |       BY num_operand { $$ = $2; }
                ;

reserved_value: spaces_etc
        |       ZERO        { $$ = ZERO; }
        |       NULLS       { $$ = NULLS; }
                ;
spaces_etc:     SPACES      { $$ = SPACES; }
        |       HIGH_VALUES { $$ = HIGH_VALUES; }
        |       LOW_VALUES  { $$ = LOW_VALUES; }
        |       QUOTES      { $$ = QUOTES; }
                ;

variable_type:  NUMERIC           { $$ = NUMERIC; }
        |       ALPHABETIC        { $$ = ALPHABETIC; }
        |       ALPHABETIC_LOWER  { $$ = ALPHABETIC_LOWER; }
        |       ALPHABETIC_UPPER  { $$ = ALPHABETIC_UPPER; }
        |       DBCS              { $$ = DBCS; }
        |       KANJI             { $$ = KANJI; }
                ;

subtract:       subtract_impl end_subtract  { ast_subtract($1); }
        |       subtract_cond end_subtract  { ast_subtract($1); }
                ;
subtract_impl:  SUBTRACT subtract_body[body]
                {
                  statement_begin(@1, SUBTRACT);
                  $$ = $body;
                }
                ;
subtract_cond:  SUBTRACT subtract_body[body] arith_errs[err]
                {
                  statement_begin(@1, SUBTRACT);
                  $body->on_error = $err.on_error;
                  $body->not_error = $err.not_error;
                  $$ = $body;
                }
                ;
end_subtract:   %empty %prec SUBTRACT
        |       END_SUBTRACT
                ;

subtract_body:  sum FROM rnames
                {
                  $$ = new arith_t(no_giving_e, $sum);
                  std::copy( rhs.begin(),
                             rhs.end(), back_inserter($$->tgts) );
                  rhs.clear();
                }
        |       sum FROM num_operand[input] GIVING rnames
                {
                  $$ = new arith_t(giving_e, $sum);
                  $$->B.push_back(*$input);
                  std::copy( rhs.begin(),
                             rhs.end(), back_inserter($$->tgts) );
                  rhs.clear();
                }
        |       CORRESPONDING sum FROM rnames
                {
                  corresponding_fields_t pairs =
                    corresponding_arith_fields( $sum->refers.front().field,
                                                rhs.front().refer.field );
                    if( pairs.empty() ) {
                      yywarn( "%s and %s have no corresponding fields",
                               $sum->refers.front().field->name,
                               rhs.front().refer.field->name );
                    }
                  // First src/tgt elements are templates.
                  // Their subscripts apply to the correspondents.
                  $$ = new arith_t(corresponding_e, $sum);
                  $$->tgts.push_front(rhs.front());
                  // use arith_t functor to populate A and tgts
                  *$$ = std::for_each( pairs.begin(), pairs.end(), *$$ );
                  $$->A.pop_front();
                  $$->tgts.pop_front();
                  rhs.clear();
                }
                ;

vargs:          varg { $$ = new vargs_t($varg); }
        |       vargs[args] varg { $args->push_back($varg); $$ = $args; }
                ;

varg:           varg1
        |       ALL varg1 { $$ = $2; $$->all = true; }
                ;

varg1:          scalar
	|	varg1a
		;
varg1a:         ADDRESS OF scalar {
		  $$ = $scalar;
		  $$->addr_of = true;
		}
        |       intrinsic_call
        |       literal
                {
                  $$ = new_reference($1);
                }
        |       reserved_value
                {
                  $$ = new_reference(constant_of(constant_index($1)));
                }
        |       LENGTH_OF name[val] {
                  location_set(@1);
                  $$ = new cbl_refer_t( new_tempnumeric() );
		  $$->field->clear_attr(signable_e);
                  if( dialect_gcc() ) {
		    dialect_error(@1, "LENGTH OF", "ibm");
                  }
		  parser_set_numeric($$->field, $val->size());
		}
        |       LENGTH_OF name[val] subscripts[subs] {
                  location_set(@1);
                  $$ = new cbl_refer_t( new_tempnumeric() );
		  $$->field->clear_attr(signable_e);
                  if( dialect_gcc() ) {
		    dialect_error(@1, "LENGTH OF", "ibm");
                  }
                  if( 0 == dimensions($val) ) {
		    cbl_refer_t r1($val);
		    subscript_dimension_error( @subs, $subs->refers.size(), &r1 );
                  }
		  parser_set_numeric($$->field, $val->data.capacity);
                }
                ;

literal:        literalism
                {
                  $$ = $1.isymbol()?
                    cbl_field_of(symbol_at($1.isymbol()))
                    :
                    new_literal($1, quoted_e);
                }
        |       NUMSTR
                {
                  $$ = new_literal($1.string, $1.radix);
                }
        |       DATETIME_FMT
                {
                  $$ = new_literal(strlen($1), $1, quoted_e);
                }
        |       DATE_FMT
                {
                  $$ = new_literal(strlen($1), $1, quoted_e);
                }
        |       TIME_FMT
                {
                  $$ = new_literal(strlen($1), $1, quoted_e);
                }
                ;

raise:          RAISE EXCEPTION NAME
                {
                  auto ec = ec_type_of($NAME);
                  if( ec == ec_none_e ) {
                    error_msg(@NAME, "not an EXCEPTION CONDITION: %s", $NAME);
                    YYERROR;
                  }
                  statement_begin(@$, RAISE);
                  parser_exception_raise(ec);
                }
        |       RAISE NAME
                {
                  auto ec = ec_type_of($NAME);
                  if( ec != ec_none_e ) {
                    error_msg(@NAME, "RAISE EXCEPTION required for "
			    "EXCEPTION CONDITION: %s", $NAME);
                    YYERROR;
                  }
                  cbl_unimplemented("RAISE <EXCEPTION OBJECT>");
                  YYERROR;
                }
                ;

read:           read_file
                {
                  current.declaratives_evaluate($1.file, $1.handled);
                }
                ;

read_file:      READ read_body {
                  file_read_args.call_parser_file_read();
                  $$.file = $2; $$.handled = FsSuccess;
                }
        |       READ read_body END_READ {
                  file_read_args.call_parser_file_read();
                  $$.file = $2; $$.handled = FsSuccess;
                }
        |       READ read_body read_eofs[err] {
                  bool handled = $err.nclause == 2 || !$err.tf;
                  $$.file = $2; $$.handled = handled? FsEofSeq : FsSuccess;
                  if( $$.file->access == file_access_rnd_e ) {
                    // None of ADVANCING, AT END, NEXT, NOT AT END, or PREVIOUS
                    // shall be specified if ACCESS MODE RANDOM
                    error_msg(@err, "%s: AT END invalid for ACCESS MODE RANDOM", $$.file->name);
                    YYERROR;
                  }
                  parser_fi();
                }
        |       READ read_body read_eofs[err] END_READ {
                  bool handled = $err.nclause == 2 || !$err.tf;
                  $$.file = $2; $$.handled = handled? FsEofSeq : FsSuccess;
                  if( $$.file->access == file_access_rnd_e ) {
                    error_msg(@err, "%s: AT END invalid for ACCESS MODE RANDOM", $$.file->name);
                    YYERROR;
                  }
                  parser_fi();
                }
        |       READ read_body io_invalids[err] {
                  bool handled = $err.nclause == 2 || !$err.tf;
                  $$.file = $2; $$.handled = handled? FsNotFound : FsSuccess;
                  parser_fi();
                }
        |       READ read_body io_invalids[err] END_READ {
                  bool handled = $err.nclause == 2 || !$err.tf;
                  $$.file = $2; $$.handled = handled? FsNotFound : FsSuccess;
                  parser_fi();
                }
                ;

read_body:      NAME read_next read_into read_key
                {
                  statement_begin(@$, READ);
                  struct symbol_elem_t *e = symbol_file(PROGRAM, $NAME);
                  if( !e ) {
                    error_msg(@1, "invalid file name '%s'", $NAME);
                    YYERROR;
                  }

                  $$ = cbl_file_of(e);

                  struct cbl_field_t *record = symbol_file_record($$);
                  if( !record ) {
                    error_msg(@1, "syntax error? invalid file record name");
                    YYERROR;
                  }
                  if( 0 && $$->access == file_access_dyn_e && $read_next >= 0 ) {
                    error_msg(@1, "sequential DYNAMIC access requires NEXT RECORD");
                    YYERROR;
                  }
                  if( $read_key->field && is_sequential($$) ) {
                    error_msg(@1, "SEQUENTIAL file %s has no KEY", $$->name);
                    YYERROR;
                  }
                  if( $$->org == file_line_sequential_e && $read_next == -2 ) {
                    error_msg(@1, "LINE SEQUENTIAL file %s cannot READ PREVIOUS",
                             $$->name);
                    YYERROR;
                  }
                  if( $read_key->field && $read_next < 0 ) {
                    error_msg(@1, "cannot read NEXT with KEY", $$->name);
                    YYERROR;
                  }

                  int ikey = $read_next;
                  if( $read_key->field ) {
                    ikey = $$->key_one($read_key->field);
                  }

                  file_read_args.init( $$, record, $read_into, ikey );
                }
                ;

read_next:      %empty          { $$ =  0; }
        |       PREVIOUS RECORD { $$ = -2; }
        |       PREVIOUS        { $$ = -2; }
        |       NEXT     RECORD { $$ = -1; }
        |       NEXT            { $$ = -1; }
        |                RECORD { $$ =  0; }
                ;

read_into:      %empty      { $$ = NULL; }
        |       INTO scalar { $$ = $scalar; }
                ;

                /*
                 * read_eofs may have 1 or 2 clauses, plus a boolean that
                 * represents whether the last one is a NOT clause.  That is,
                 * there's an AT END clause if there are 2 clauses, or if
                 * there's one clause that is an AT END clause (tf is false).
                 */
read_eofs:      read_eof  { $$.nclause = 1; $$.tf = $1; }
        |       read_eofs read_eof
                {
                  $$ = $1;
                  if( ++$$.nclause > 2 ) {
                    error_msg(@2, "too many AT END conditions");
                    YYERROR;
                  }
                  if( $$.tf == $read_eof ) {
                    error_msg(@2, "duplicate AT END conditions");
                    YYERROR;
                  }
                  parser_fi();
                }
                ;

read_eof:       END
                {
                  if( file_read_args.ready() ) {
                    file_read_args.default_march(true);
                    file_read_args.call_parser_file_read();
                  }

                  static const struct status_t { file_status_t L, U; }
                        at_end = { FsEofSeq, FsKeySeq },
                    not_at_end = { FsSuccess, FsEofSeq };
                  assert( $1 == END || $1 == NOT );
                  status_t st = $1 == END? at_end : not_at_end;
                  //  L <= ec < U
                  cbl_field_t *cond = ast_file_status_between(st.L, st.U);

                  parser_if(cond);
                  parser_exception_clear();
                } statements {
                  parser_else();
                  $$ = $1 == NOT;
                }
                ;

write_eops:     write_eop  { $$.nclause = 1; $$.tf = $1; }
        |       write_eops write_eop
                {
                  $$ = $1;
                  if( ++$$.nclause > 2 ) {
                    error_msg(@2, "too many AT EOP conditions");
                    YYERROR;
                  }
                  if( $$.tf == $write_eop ) {
                    error_msg(@2, "duplicate AT EOP conditions");
                    YYERROR;
                  }
                }
                ;

write_eop:      EOP
                {
                  // cond represents the _FILE_STATUS of the last WRITE.
                  static cbl_field_t *cond = constant_of(constant_index(ZERO));

                  if( file_write_args.ready() ) {
                    file_write_args.call_parser_file_write(true);
                    cond = ast_file_status_between(FsEofSeq, FsKeySeq);
                  }
                  assert( $1 == EOP || $1 == NOT );
                  if( $1 == NOT ) {
                    parser_logop(cond, NULL, not_op, cond);
                  }
                  parser_if(cond);
                  parser_exception_clear();
                } statements {
                  parser_else();
                  parser_fi();
                  $$ = $1 == NOT;
                }
                ;

read_key:       %empty      { $$ = new cbl_refer_t();  }
        |       KEY is name { $$ = new cbl_refer_t($name); }
                ;

write:          write_file
                {
                  current.declaratives_evaluate( $1.file, $1.handled );
                }
                ;

write_file:     WRITE write_body
                {
                  $$.file = $2; $$.handled = FsSuccess;
                  bool sequentially = $$.file->access == file_access_seq_e;
                  file_write_args.call_parser_file_write(sequentially);
                }
        |       WRITE write_body END_WRITE
                {
                  $$.file = $2; $$.handled = FsSuccess;
                  bool sequentially = $$.file->access == file_access_seq_e;
                  file_write_args.call_parser_file_write(sequentially);
                }
        |       WRITE write_body write_eops[err] {
                  bool handled = $err.nclause == 2 || !$err.tf;
                  $$.file = $2; $$.handled = handled? FsEofSeq : FsSuccess;
                }
        |       WRITE write_body write_eops[err] END_WRITE {
                  bool handled = $err.nclause == 2 || !$err.tf;
                  $$.file = $2; $$.handled = handled? FsEofSeq : FsSuccess;
                }
        |       WRITE write_body io_invalids[err] {
                  bool handled = $err.nclause == 2 || !$err.tf;
                  $$.file = $2; $$.handled = handled? FsEofSeq : FsSuccess;
                  parser_fi();
                }
        |       WRITE write_body io_invalids[err] END_WRITE {
                  bool handled = $err.nclause == 2 || !$err.tf;
                  $$.file = $2; $$.handled = handled? FsEofSeq : FsSuccess;
                  parser_fi();
                }
                ;

write_body:     write_what[field] advance_when[when] advancing
                {
                  statement_begin(@$, WRITE);
                  cbl_file_t *file = symbol_record_file($field);
                  if( !file ) {
                    error_msg(@1, "no FD record found for %s", $field->name);
                    YYERROR;
                  }
                  $$ = file_write_args.init( file, $field, $when==AFTER, $advancing );
                  current.declaratives_evaluate( file );
                }
        |       write_what[field]
                {
                  statement_begin(@$, WRITE);
                  cbl_file_t *file = symbol_record_file($field);
                  if( !file ) {
                    error_msg(@1, "no FD record found for %s", $field->name);
                    YYERROR;
                  }
		  cbl_refer_t lines;
                  switch(file->org) {
                  case file_sequential_e:
                    break;
                  case file_line_sequential_e:
                    lines.field = literally_one;
                    break;
                  case file_disorganized_e:
                  case file_indexed_e:
                  case file_relative_e:
                    break;
                  }
                  $$ = file_write_args.init( file, $field, false, &lines );
                }
                ;
write_what:     file_record FROM alpha_val[input]
                {
                  $$ = $1;
                  parser_move($$, *$input);
                }
        |       file_record
                ;
file_record:    NAME
                {
                  name_queue.qualify(@1, $1);
		  auto namelocs( name_queue.pop() );
		  auto names( name_queue.namelist_of(namelocs) );
		  auto inner = namelocs.back();
                  if( ($$ = field_find(names)) == NULL ) {
                    error_msg(inner.loc, "no record name '%s'", inner.name);
                    YYERROR;
                  }
                }
        |       NAME inof filename
                {
                  std::list<const char *> names = {$filename->name, $NAME};
                  auto record = symbol_find(names);
                  if( !record ) {
                    error_msg(@$, "%s IN %s not found",
                             $NAME, $filename->name);
                    YYERROR;
                  }
                  $$ = cbl_field_of(record);
                }
        |       FILE_KW filename
                {
                  $$ = cbl_field_of(symbol_at($filename->default_record));
                }
                ;
advance_when:   BEFORE { $$ = BEFORE; }
        |       AFTER  { $$ = AFTER; }
                ;

advancing:      advance_by
        |       ADVANCING advance_by { $$ = $2; }
                ;
advance_by:     scalar lines { $$ = $1; } /* BUG: should accept reference */
        |       signed_literal lines { $$ = new_reference($1); }
        |       PAGE
                {
                  /*
                   * The standard says behavior is undefined when the
                   * number of lines is negative.  So, we use the
                   * negative Number Of The Beast as a PAGE flag.
                   */
                  $$ = new_reference( new_literal("-666") );
                }
        |       device_name { $$ = new_reference(literally_one); }
                ;

io_invalids:    io_invalid { $$.nclause = 1; $$.tf = $io_invalid; }
        |       io_invalids io_invalid
                {
                  $$ = $1;
                  if( ++$$.nclause > 2 ) {
                    error_msg(@2, "too many INVALID clauses");
                    YYERROR;
                  }
                  if( $$.tf == $io_invalid ) {
                    error_msg(@2, "duplicate INVALID conditions");
                    YYERROR;
                  }
                  parser_fi();
                }
                ;

io_invalid:     INVALID key {
                  if( file_delete_args.ready() ) {
                    file_delete_args.call_parser_file_delete(false);
                  }
                  if( file_read_args.ready() ) {
                    file_read_args.default_march(false);
                    file_read_args.call_parser_file_read();
                  }
                  if( file_rewrite_args.ready() ) {
                    file_rewrite_args.call_parser_file_rewrite(false);
                  }
                  if( file_start_args.ready() ) {
                    file_start_args.call_parser_file_start();
                  }
                  if( file_write_args.ready() ) {
                    file_write_args.call_parser_file_write(false);
                  }

                  static const struct status_t { file_status_t L, U; }
                        invalid = { FsKeySeq, FsOsError },
                    not_invalid = { FsSuccess, FsEofSeq };
                  assert( $1 == INVALID || $1 == NOT );
                  status_t st = $1 == INVALID? invalid : not_invalid;
                  //  L <= ec < U
                  cbl_field_t *cond = ast_file_status_between(st.L, st.U);

                  parser_if(cond);
                  parser_exception_clear();
                } statements {
                  parser_else();
                  $$ = $1 == NOT;
                }
                ;

delete:         delete_impl  end_delete
        |       delete_cond  end_delete
                ;
delete_impl:    DELETE delete_body[file]
                {
                  file_delete_args.call_parser_file_delete(true);
                  current.declaratives_evaluate( $file );
                }
                ;
delete_cond:    DELETE delete_body[file] io_invalids
                {
                  if( is_sequential($file) ) {
                    error_msg(@2, "INVALID KEY phrase invalid for sequential file '%s'",
                             $file->name);
                    YYERROR;
                  }
                  if( $file->access == file_access_seq_e ) {
                    error_msg(@2, "INVALID KEY phrase invalid for "
                             "sequential access mode on '%s'",
                             $file->name);
                    YYERROR;
                  }
                  parser_fi();
                  // call happens in io_invalid
                  current.declaratives_evaluate( $file );
                }
                ;

delete_body:    filename[file] record
                {
                  statement_begin(@1, DELETE);
                  file_delete_args.init( $file );
                  $$ = $file;
                }
                ;
end_delete:     %empty %prec DELETE
        |       END_DELETE
                ;

rewrite:        rewrite1
                {
                  current.declaratives_evaluate($1.file, $1.handled);
                }
                ;

rewrite1:       REWRITE rewrite_body end_rewrite {
                  $$.file = $2.file; $$.handled = FsSuccess;
                  file_rewrite_args.call_parser_file_rewrite( true );
                }
        |       REWRITE rewrite_body io_invalids[err] end_rewrite {
                  bool handled = $err.nclause == 2 || !$err.tf;
                  $$.file = $2.file; $$.handled = handled? FsNotFound : FsSuccess;

                  if( is_sequential($$.file) ) {
                    error_msg(@2, "INVALID KEY for sequential file '%s'",
                             $$.file->name);
                    YYERROR;
                  }
                  if( $$.file->relative_sequential() ) {
                    error_msg(@2, "%s: INVALID KEY may not be specified for "
                             "RELATIVE file and SEQUENTIAL access",
                             $$.file->name);
                    YYERROR;
                  }
                  parser_fi();
                }
                ;

rewrite_body:   write_what record
                {
                  statement_begin(@$, REWRITE);
                  symbol_elem_t *e = symbol_file(PROGRAM, $1->name);
                  file_rewrite_args.init(cbl_file_of(e), $1);
                  $$.file = cbl_file_of(e);
                  $$.buffer = $1;
                }
                ;
end_rewrite:    %empty %prec REWRITE
        |       END_REWRITE
                ;

start:          start_impl end_start
        |       start_cond end_start
                ;
start_impl:     START start_body
                ;
start_cond:     START start_body io_invalids {
                  parser_fi();
                }
                ;
end_start:      %empty %prec START
        |       END_START
                ;

start_body:     filename[file]
                {
                  statement_begin(@$, START);
                  file_start_args.init(@file, $file);
                  parser_file_start( $file, lt_op, 0 );
                }
        |       filename[file] KEY relop name[key]
                { // lexer swallows IS, although relop allows it.
                  statement_begin(@$, START);
                  int key = $file->key_one($key);
                  int size = key == 0 ? 0 : $file->keys[key - 1].size();
                  auto ksize = new_tempnumeric();
                  parser_set_numeric(ksize, size);
                  if( yydebug ) {
                    yywarn("START: key #%d '%s' has size %d",
                          key, $key->name, size);
                  }
                  file_start_args.init(@file, $file);
                  parser_file_start( $file, relop_of($relop), key, ksize );
                }
        |       filename[file] KEY relop name[key] with LENGTH expr
                { // lexer swallows IS, although relop allows it.
                  statement_begin(@$, START);
                  int key = $file->key_one($key);
                  file_start_args.init(@file, $file);
                  parser_file_start( $file, relop_of($relop), key, *$expr );
                }
        |       filename[file] FIRST
                {
                  statement_begin(@$, START);
                  file_start_args.init(@file, $file);
                  parser_file_start( $file, lt_op, -1 );
                }
        |       filename[file] LAST
                {
                  statement_begin(@$, START);
                  file_start_args.init(@file, $file);
                  parser_file_start( $file, gt_op, -2 );
                }
                ;

merge:          MERGE { statement_begin(@1, MERGE); }
                filename[file] sort_keys sort_seq
                  USING filenames[inputs] sort_output
                {
                  std::vector <cbl_key_t> keys($sort_keys->key_list.size());
		  std::copy( $sort_keys->key_list.begin(),
			     $sort_keys->key_list.end(), keys.begin() );

                  size_t ninput = $inputs->files.size();
                  size_t noutput = $sort_output->nfile();
                  cbl_file_t **inputs = NULL, **outputs = NULL;
                  cbl_perform_tgt_t *out_proc = NULL;

                  inputs = new cbl_file_t * [ ninput ];
                  std::copy($inputs->files.begin(),
                            $inputs->files.end(), inputs);

                  if( noutput > 0 ) {
                    outputs = new cbl_file_t * [ noutput ];
                    std::copy($sort_output->file_list.files.begin(),
                              $sort_output->file_list.files.end(), outputs);
                  } else {
                    out_proc = &$sort_output->tgt;
                  }

                  parser_file_merge( $file, $sort_seq,
                                     keys.size(), keys.empty()? NULL : keys.data(),
                                     ninput, inputs,
                                     noutput, outputs,
                                     out_proc );
                }
                ;

set_tgts:       set_tgt {
                  $$ = new tgt_list_t;
                  list_add($$->targets, *$set_tgt, current_rounded_mode());
                }
        |       set_tgts set_tgt
                {
                  list_add($1->targets, *$set_tgt, current_rounded_mode());
                }
                ;
set_operand:    set_tgt
        |       signed_literal { $$ = new_reference($1); }
        |       ADDRESS of FUNCTION ctx_name[name]
                {
                  $$ = NULL;
                  auto e = symbol_function(0, $name);
                  if( e ) {
                    $$ = new cbl_refer_t(cbl_label_of(e));
                  } else {
                    e = symbol_find(@name, $name);
                    if( !e ) {
                      error_msg(@name, "%s not found", $name);
                      YYERROR;
                    }
                    $$ = new cbl_refer_t(cbl_field_of(e));
                  }
                  assert($$);
                }
        |       ADDRESS of PROGRAM_kw ctx_name[name]
                {
                  $$ = NULL;
                  auto label = symbol_program(0, $name);
                  if( label ) {
                    $$ = new cbl_refer_t(label);
                  } else {
                    auto e = symbol_find(@name, $name);
                    if( !e ) {
                      error_msg(@name, "%s not found", $name);
                      YYERROR;
                    }
                    $$ = new cbl_refer_t(cbl_field_of(e));
                  }
                  assert($$);
                }
        |       ADDRESS of PROGRAM_kw LITERAL[lit]
                {
                  auto label = symbol_program(0, $lit.data);
                  $$ = new cbl_refer_t( label );
                }
                ;
set_tgt:        scalar
        |       ADDRESS of scalar { $$ = $scalar; $$->addr_of = true; }
                ;

set:            SET set_tgts[tgts] TO set_operand[src]
                {
                  statement_begin(@1, SET);

                  switch( set_operand_type(*$src) ) {
                  case FldInvalid:
                    if( ! ($src->prog_func && $src->addr_of) ) {
                      error_msg(@src, "SET source operand '%s' is invalid", $src->name());
                      YYERROR;
                      break;
                    }
                    __attribute__((fallthrough));
                  case FldPointer:
                    if( !valid_set_targets(*$tgts, true) ) {
                      YYERROR;
                    }
                    ast_set_pointers($tgts->targets, *$src);
                    break;

                  case FldIndex:
                  case FldPacked:
                  case FldNumericDisplay:
                  case FldNumericBinary:
                  case FldFloat:
                  case FldNumericBin5:
                  case FldLiteralN:
                    if( !valid_set_targets(*$tgts, $src->is_pointer()) ) {
                      YYERROR;
                    }
                    parser_index($tgts, *$src);
                    break;
                  default:
                    if( strcmp($src->field->name, "ZEROS") != 0 ) {
                      error_msg(@src, "%s must be numeric or POINTER type",
                              $src->field->name);
                      YYERROR;
                    }
                  }
                }
        |       SET set_tgts[tgts] TO NULLS[src]
                {
                  statement_begin(@1, SET);
                  if( !valid_set_targets(*$tgts, true) ) {
                    YYERROR;
                  }
                  ast_set_pointers($tgts->targets, constant_of(constant_index(NULLS)));
                }
        |       SET set_tgts TO spaces_etc[error]
                {
                  error_msg(@2, "invalid value for SET TO");
                }
        |       SET set_tgts[tgts] TO ENTRY scalar[src]
                {
                  ast_set_pointers($tgts->targets, *$src);
                }
        |       SET set_tgts[tgts] TO ENTRY LITERAL[src]
                {
                  auto literal = $src.isymbol()?
                    cbl_field_of(symbol_at($src.isymbol()))
                    :
                    new_literal($src, quoted_e);
                  ast_set_pointers($tgts->targets, literal);
                }
        |       SET set_tgts[tgts] UP BY num_operand[src]
                {
                  statement_begin(@1, SET);
                  list<cbl_num_result_t>& tgts = $tgts->targets;

                  for( auto p = tgts.begin(); p != tgts.end(); p++ ) {
                    parser_add2( *p, *$src );
                  }
                  delete $tgts;
                }
        |       SET set_tgts[tgts] DOWN BY num_operand[src]
                {
                  statement_begin(@1, SET);
                  list<cbl_num_result_t>& tgts = $tgts->targets;

                  for( auto p = tgts.begin(); p != tgts.end(); p++ ) {
                    parser_subtract2( *p, *$src );
                  }
                  delete $tgts;
                }
        |       SET ENVIRONMENT envar TO alpha_val[scalar]
                {
                  statement_begin(@1, SET);
                  parser_set_envar(*$envar, *$scalar);
                }
        |       SET LAST EXCEPTION TO OFF
                {
                  statement_begin(@1, SET);
                  // send the signal to clear the stashed exception values
                  parser_exception_raise(ec_none_e);
                }
        |       SET LENGTH_OF scalar TO scalar
                {
                  statement_begin(@1, SET);
                  cbl_unimplemented("SET LENGTH OF");
                  YYERROR;
                }
        |       SET scalar88s[names] TO true_false[yn]
                {
                  statement_begin(@1, SET);
                  class set_conditional {
                    bool tf;
                   public:
                    set_conditional( int token ) : tf(token == TRUE_kw) {}
                    void operator()(cbl_refer_t& refer) {
                      if( refer.field->data.false_value_of() == NULL && !tf ) {
			auto loc = symbol_field_location(field_index(refer.field));
                        error_msg(loc, "%s has no WHEN SET TO FALSE",
                                 refer.field->name);
                        return;
                      }
                      parser_set_conditional88(refer, tf);
                    }
                  };
                  std::for_each($names->refers.begin(), $names->refers.end(),
                                set_conditional($yn));
                }
        |       SET { statement_begin(@1, SET); } many_switches
                ;

many_switches:                set_switches
        |       many_switches set_switches
                ;

set_switches:   switches TO on_off
                {
                  struct switcheroo {
                    bitop_t op;
                    switcheroo( bool tf ) : op( tf? bit_set_op : bit_clear_op ) {}
                    switcheroo& operator()(cbl_field_t* sw) {
                      assert(sw->type == FldSwitch);
                      assert(sw->data.initial); // not a switch condition
                      parser_bitop(NULL, parent_of(sw),
                                   op, sw->data.upsi_mask_derive());
                      return *this;
                    }
                  };
                  std::for_each( $switches->fields.begin(), $switches->fields.end(),
                                 switcheroo($on_off) );
                }
                ;

switches:       one_switch              { $$ = new field_list_t($1); }
        |       switches one_switch[sw] { $$->fields.push_back($sw); }
                ;
one_switch:     SWITCH {
                  $$ = cbl_field_of(symbol_find(@1, $1));
                }
                ;

on_off:         ON  { $$ = true; }
        |       OFF { $$ = false; }
        ;

search:         search_linear end_search
        |       search_binary end_search
                ;

search_linear:  SEARCH search_1_place search_1_cases
                {
                  parser_lsearch_end(search_current());
                  search_free();
                }
                ;
end_search:     %empty %prec SEARCH
        |       END_SEARCH
                ;

search_1_place: search_1_body
        |       search_1_body at END statements
                ;

search_1_body:  name[table] search_varying[varying]
                {
                  statement_begin(@$, SEARCH);
                  cbl_field_t *index = table_primary_index($table);
                  if( !index ) {
                    error_msg(@1, "%s has no defined index", $table->name);
                    YYERROR;
                  }

                  cbl_name_t label_name;
		  auto len = snprintf(label_name, sizeof(label_name),
				     "linear_search_%d", yylineno);
		  if( ! (0 < len && len < int(sizeof(label_name))) ) {
		    gcc_unreachable();
		  }
                  cbl_label_t *name = label_add( LblSearch,
                                                 label_name, yylineno );
		  auto varying($varying);
                  if( index == varying ) varying = NULL;
                  parser_lsearch_start( name, $table, index, varying );
                  search_alloc(name);
                }
                ;

search_varying: %empty         { $$ = NULL; }
        |       VARYING name   { $$ = $2; }
                ;

search_1_cases: search_1_case
                {
                  if( yydebug ) {
                    const char *lookahead = "?";
                    switch( yychar ) {
                    case 0:   lookahead = "YYEOF"; break;
                    case -2: lookahead = "YYEMPTY"; break;
                    default:
                      if( yychar > 0 ) {
                        lookahead = keyword_str(yychar);
                      }
                    }
                    yywarn("Just one case, lookahead is '%s'", lookahead);
                  }
                }
        |       search_1_cases search_1_case
                ;
search_1_case:  search_1_when search_1_test search_stmts
                ;
search_1_when:  WHEN { parser_lsearch_conditional(search_current()); }
                ;
search_1_test:  bool_expr {
                  parser_lsearch_when( search_current(), $bool_expr->cond() );
                }
                ;

search_binary:  SEARCH ALL search_2_body search_2_cases
                {
                  parser_bsearch_end(search_current());
                  search_free();
                }
        |       SEARCH ALL search_2_body at END statements search_2_cases
                {
                  parser_bsearch_end(search_current());
                  search_free();
                }
                ;

search_2_body:  name[table]
                {
                  statement_begin(@$, SEARCH);
                  char *label_name = xasprintf("binary_search_%d", yylineno);
                  cbl_label_t *name = label_add( LblSearch,
                                                 label_name, yylineno );
                  parser_bsearch_start( name, $table );
                  search_alloc(name);
                }
                ;

search_2_cases: search_2_case
        |       search_2_cases search_2_case
                ;
search_2_case:  WHEN { parser_bsearch_conditional(search_current()); }
                search_terms search_stmts
                ;

search_stmts:   statements    %prec ADD
        |       NEXT SENTENCE %prec ADD {
                  next_sentence = label_add(LblNone, "next_sentence", 0);
                  parser_label_goto(next_sentence);
                }
                ;

search_terms:   search_term
        |       search_terms AND search_term
                ;
search_term:    scalar[key] '=' search_expr[sarg]
                {
                  if( $key->nsubscript == 0 ) {
                    error_msg(@1, "no index for key");
                    YYERROR;
                  }
                  if( dimensions($key->field) < $key->nsubscript ) {
                    error_msg(@1, "too many subscripts: "
                              "%zu for table of %zu dimensions",
                              $key->nsubscript, dimensions($key->field) );
                    YYERROR;
                  }

                  parser_bsearch_when(  search_current(),
                                        *$key,
                                        *$sarg,
                                        is_ascending_key(*$key) );
                }
        |       scalar88[sarg] {
                  cbl_field_t *key = field_at($sarg->field->parent);
                  parser_bsearch_when( search_current(), key, *$sarg,
                                       is_ascending_key(key) );
                }
                ;
search_expr:    expr
        |       LITERAL { $$ = new_reference(new_literal($1, quoted_e)); }
                ;

sort:           sort_table
        |       sort_file
                ;

sort_table:     SORT tableref[table] sort_keys sort_dup sort_seq {
                  statement_begin(@1, SORT);
                  std::vector <cbl_key_t> keys($sort_keys->key_list.size());
		  if( ! is_table($table->field) ) {
		    error_msg(@1, "%s has no OCCURS clause", $table->field->name);
		  }
                  // 23) If data-name-1 is omitted, the data item referenced by
                  // data-name-2 is the key data item.
		  int i = 0;
                  for( auto k : $sort_keys->key_list ) {
                    if( k.fields.empty() ) {
                      k.fields.push_back($table->field);
                    }
                    keys.at(i++) = cbl_key_t(k);
                  }

                  parser_sort( *$table, $sort_dup, $sort_seq,
			       keys.size(), keys.empty()? NULL : keys.data() );
                }
        |       SORT tableref[table] sort_dup sort_seq {
                  statement_begin(@1, SORT);
		  if( ! is_table($table->field) ) {
		    error_msg(@1, "%s has no OCCURS clause", $table->field->name);
		  }
                  cbl_key_t
                    key = cbl_key_t($table->field->occurs.keys[0]),
                    guess(1, &$table->field);
                  ;
                  if( key.nfield == 0 ) key = guess;
                  parser_sort( *$table, $sort_dup, $sort_seq, 1, &key );
                }
                ;

sort_file:      SORT FILENAME[file] sort_keys  sort_dup    sort_seq
                                    sort_input sort_output
                {
                  statement_begin(@1, SORT);
                  struct symbol_elem_t *e = symbol_file(PROGRAM, $file);
                  if( !(e && e->type == SymFile) ) {
                    error_msg(@file, "invalid file name");
                    YYERROR;
                  }
                  cbl_file_t *file = cbl_file_of(e);
                  std::vector <cbl_key_t> keys($sort_keys->key_list.size());
		  std::copy( $sort_keys->key_list.begin(),
			     $sort_keys->key_list.end(), keys.begin() );

                  size_t ninput = $sort_input->nfile();
                  size_t noutput = $sort_output->nfile();
                  cbl_file_t **inputs = NULL, **outputs = NULL;
                  cbl_perform_tgt_t *in_proc = NULL, *out_proc = NULL;

                  if( ninput > 0 ) {
                    inputs = new cbl_file_t * [ ninput ];
                    std::copy($sort_input->file_list.files.begin(),
                              $sort_input->file_list.files.end(), inputs);
                  } else {
                    in_proc = &$sort_input->tgt;
                  }
                  if( noutput > 0 ) {
                    outputs = new cbl_file_t * [ noutput ];
                    std::copy($sort_output->file_list.files.begin(),
                              $sort_output->file_list.files.end(), outputs);
                  } else {
                    out_proc = &$sort_output->tgt;
                  }

                  parser_file_sort( file,
                                    $sort_dup,
                                    $sort_seq,
                                    keys.size(), keys.empty()? NULL : keys.data(),
                                    ninput, inputs,
                                    noutput, outputs,
                                    in_proc, out_proc );
                }
        |       SORT FILENAME[file] sort_keys sort_dup sort_seq  error
                {
                  error_msg(@file, "SORT missing INPUT or OUTPUT phrase");
                }


sort_keys:      sort_key {
                  $$ = new sort_keys_t();
                  $$->key_list.push_back(*$sort_key);
                }
        |       sort_keys sort_key { $$->key_list.push_back(*$sort_key); }
                ;

sort_key:       on forward_order key field_list  %prec NAME
                {
                  $$ = new sort_key_t( $forward_order, *$field_list );
                }
        |       on forward_order key  %prec NAME
                {
                  field_list_t flist;
                  $$ = new sort_key_t( $forward_order, flist );
                }
                ;

forward_order:  ASCENDING  { $$ = true; }
        |       DESCENDING { $$ = false; }
                ;
field_list:     name { $$ = new field_list_t($1); }
        |       field_list name { $1->fields.push_back($name); }
                ;

sort_dup:       %empty { $$ = false; }
        |       with DUPLICATES in order { $$ = true; }
                ;
sort_seq:       %empty { $$ = NULL; }
        |       collating SEQUENCE is ctx_name[name]
                {
                  symbol_elem_t *e = symbol_alphabet(PROGRAM, $name);
                  if( !e ) {
                    error_msg(@name, "not an alphabet: '%s'", $name);
                    $$ = NULL;
                  }
                  $$ = cbl_alphabet_of(e);
                }
                ;

sort_input:     USING filenames
                {
                  $$ = new file_sort_io_t(*$2);
                  delete $2;
                }
        |       INPUT PROCEDURE is sort_target
                {
                  $$ = new file_sort_io_t(*$sort_target);
                  delete $sort_target;
                }
                ;
sort_output:    GIVING filenames
                {
                  $$ = new file_sort_io_t(*$2);
                }
        |       OUTPUT PROCEDURE is sort_target
                {
                  $$ = new file_sort_io_t(*$sort_target);
                }
                ;

sort_target:    label_name
                {
                  $$ = new cbl_perform_tgt_t($1);
                }
        |       label_name THRU label_name
                {
                  $$ = new cbl_perform_tgt_t($1, $3);
                }
                ;

release:        RELEASE NAME[record] FROM scalar[name]
                {
                  statement_begin(@1, RELEASE);
                  symbol_elem_t *record = symbol_find(@record, $record);
                  parser_move(cbl_field_of(record), *$name);
                  parser_release(cbl_field_of(record));
                }
        |       RELEASE NAME[record]
                {
                  statement_begin(@1, RELEASE);
                  symbol_elem_t *record = symbol_find(@record, $record);
                  parser_release(cbl_field_of(record));
                }
                ;

return_stmt:    return_impl return_end
        |       return_cond return_end
                ;

return_impl:    RETURN return_body[body]
                {
                  cbl_file_t *file = cbl_file_of(symbol_at(current_sort_file));
                  parser_return_finish(file);
                  current_sort_file = $body;
                }
                ;

return_cond:    RETURN return_body[body] return_outputs
                {
                  cbl_file_t *file = cbl_file_of(symbol_at(current_sort_file));
                  parser_return_finish(file);
                  current_sort_file = $body;
                }
                ;
return_end:     %empty %prec RETURN
        |       END_RETURN
                ;

return_body:    return_file
                {
                  file_return_args.call_parser_return_start();
                  }
        |       return_file INTO scalar
                {
                  file_return_args.call_parser_return_start(*$scalar);
                }
                ;

return_file:    filename
                {
                  statement_begin(@$, RETURN);
                  $$ = current_sort_file; // preserve current sort file
                  current_sort_file = symbol_index(symbol_elem_of($filename));
                  file_return_args.init($filename);
                }
        |       filename RECORD
                {
                  statement_begin(@$, RETURN);
                  $$ = current_sort_file; // preserve current sort file
                  current_sort_file = symbol_index(symbol_elem_of($filename));
                  file_return_args.init($filename);
                }
                ;
return_outputs: return_output
        |       return_outputs return_output // TODO: only 2, AT END and/or NOT AT END
                ;
return_output:  output_atend statements %prec RETURN
                ;

output_atend:   END {
                  assert($1 == END || $1 == NOT);
                  auto func = $1 == END?
                    parser_return_atend : parser_return_notatend ;
                  func(cbl_file_of(symbol_at(current_sort_file)));
                }
                ;
filenames:      filename { $$ = new file_list_t($1); }
        |       filenames filename { $1->files.push_back($2); }
                ;
filename:       NAME
                {
                  struct symbol_elem_t *e = symbol_file(PROGRAM, $1);
                  if( !(e && e->type == SymFile) ) {
                    error_msg(@NAME, "invalid file name");
                    YYERROR;
                  }
                  $$ = cbl_file_of(e);
                }
                ;

label_name:     NAME
                {
                  struct cbl_label_t *label = symbol_label(PROGRAM,
                                                           LblNone, 0, $1);
                  if( !label ) { // no line number for forward declaraion
                    label = label_add(@NAME, LblNone, $1);
                  }
                  $$ = label;
                }
                ;

inspected:      scalar
        |       intrinsic_call
                ;
backward:	%empty   { $$ = false; }
	|	BACKWARD { $$ = true;  }
		;
inspect:        INSPECT backward inspected TALLYING tallies
                {
                  statement_begin(@1, INSPECT);
                  ast_inspect( *$inspected, $backward, *$tallies );
                }
        |       INSPECT backward inspected TALLYING tallies REPLACING replacements
                {
                  if( is_constant($inspected->field) ) {
                    auto name = nice_name_of($inspected->field);
                    if( !name[0] ) name = "its argument";
                    error_msg(@inspected, "INSPECT cannot write to %s", name);
                    YYERROR;
                  }
                  statement_begin(@1, INSPECT);
                  // All tallying is done before any replacing
                  ast_inspect( *$inspected, $backward, *$tallies );
                  ast_inspect( *$inspected, $backward, *$replacements );
                }
        |       INSPECT backward inspected                  REPLACING replacements
                {
                  if( is_constant($inspected->field) ) {
                    auto name = nice_name_of($inspected->field);
                    if( !name[0] ) name = "its argument";
                    error_msg(@inspected, "INSPECT cannot write to %s", name);
                    YYERROR;
                  }
                  statement_begin(@1, INSPECT);
                  ast_inspect( *$inspected, $backward, *$replacements );
                }
        |       INSPECT backward inspected CONVERTING alpha_val[match]
                                  TO  all    alpha_val[replace_oper]
                                             insp_mtquals[qual]
                {
		  if( $all ) {
		    $replace_oper->all = true;
		    if( is_literal($replace_oper->field) ) {
		      if( $replace_oper->field->data.capacity != 1 ) {
			error_msg(@all, "ALL %s must be a single character",
				 $replace_oper->field->data.initial);
			YYERROR;
		      }
		    } else {
		      error_msg(@all, "ALL must be part of a figurative constant");
		      YYERROR;
		    }
		  }
                  if( is_constant($inspected->field) ) {
                    auto name = nice_name_of($inspected->field);
                    if( !name[0] ) name = "its argument";
                    error_msg(@inspected, "INSPECT cannot write to %s", name);
                    YYERROR;
                  }
                  statement_begin(@1, INSPECT);
                  // IBM Format 4 does not show the qualifiers as optional, but
                  // they don't appear in Listing-15-1.
                  parser_inspect_conv( *$inspected, $backward,
                                       *$match,
                                       *$replace_oper,
                                       $qual->before, $qual->after );
                }
                ;

tallies:        { need_nume_set(); } tally
                {
                  $$ = new ast_inspect_list_t( *$tally );
                }
        |       tallies { need_nume_set(); } tally
                {
                  $$ = $1;
                  cbl_inspect_t& next(*$tally);

                  if( !next.tally.field ) {
                    // prior tally swallowed one too many
                    cbl_inspect_t& prior = $$->back();
                    assert(prior.nbound > 0);
                    assert(prior.opers);
                    cbl_inspect_oper_t& prior_op = prior.opers[prior.nbound - 1];

                    assert(prior_op.n_identifier_3 > 0 );
                    next.tally = prior_op.matches[--prior_op.n_identifier_3].matching;
                  }
                  if( !next.tally.field ) {
                    error_msg(@$, "missing summation field before FOR");
                    YYERROR;
                  }
                  $$->push_back(next);
                }
                ;

                /*
                 * numref might be "empty" only because it was consumed by a
                 * prior insp_mtquals, which can end in a scalar. If that
                 * happens, the tallies target, above, takes back the borrowed
                 * scalar and assigns it to be the tally total, as the user
                 * intended.
                 */
tally:          numeref[total] FOR tally_fors[fors]
                { // reduce ast_inspect_t to cbl_inspect_t
                  if( yydebug && !$total ) {
                    error_msg(@FOR, "caution: missing summation field before FOR");
                  }
                  cbl_refer_t total( $total? *$total : cbl_refer_t() );
                  $$ = new cbl_inspect_t( total, $fors->opers() );
                }
                ;

tally_fors:     tally_forth
                { // reduce ast_inspect_oper_t to cbl_inspect_oper_t
                  cbl_inspect_oper_t oper( $1->bound, $1->matches );
                  $$ = new ast_inspect_t;
                  $$ ->push_back(oper);
                }
        |       tally_fors tally_forth
                {
                  cbl_inspect_oper_t oper( $2->bound, $2->matches );
                  $1 ->push_back(oper);
                }
                ;

tally_forth:    CHARACTERS insp_mtquals[q] scalar[next_tally]
                {
                  // Add ensuing scalar as if it were an argument to CHARACTERS.
                  // It will be moved to the succeeding FOR as its tally.
                  $q->matching = *$next_tally;
                  $$ = new ast_inspect_oper_t(*$q);
                }
        |       CHARACTERS insp_mtquals[q]
                {
                  $$ = new ast_inspect_oper_t(*$q);
                }
        |       ALL tally_matches[q]
                { $q->bound = bound_all_e;
                  $$ = $q;
                }
        |       LEADING tally_matches[q]
                { $q->bound = bound_leading_e;
                  $$ = $q;
                }
        |       TRAILING tally_matches[q]
                { $q->bound = bound_trailing_e;
                  $$ = $q;
		  if( ! dialect_mf() ) {
		    dialect_error(@1, "TRAILING", "mf");
		  }
                }
                ;

tally_matches:  tally_match { $$ = new ast_inspect_oper_t(*$1); }
        |       tally_matches tally_match
                { // add to the list of matches for an operand
                  $1->matches.push_back(*$2);
                }
                ;
tally_match:    alpha_val[matching] insp_mtquals[q]
                { // include the matching field with the qualifiers
                  $$ = $q;
                  $$->matching = *$matching;
                }
                ;

numeref:        %empty { $$ = NULL; need_nume_set(false); }
        |       nume[name] subscripts[subs]
                {
                  size_t n = $subs->size();
                  auto offsets = new cbl_refer_t[n];
                  std::copy( $subs->begin(), $subs->end(), offsets );
                  $$ = new cbl_refer_t($name, n, offsets);
                }
        |       nume { $$ = new cbl_refer_t($nume); }
                ;

nume:           qnume {
                  $$ = NULL;
                  struct symbol_elem_t *e = NULL;
                  size_t index = 0;
		  auto names( name_queue.pop() );

                  for( ; !names.empty(); names.pop_front() ) {
		    auto nameloc = names.front();
                    if( (e = symbol_field(PROGRAM,
                                          index, nameloc.name)) == NULL ) {
                      error_msg(nameloc.loc, "DATA-ITEM '%s' not found", nameloc.name );
                      YYERROR;
                    }
                    $$ = cbl_field_of(e);
                    index = symbol_index(e);
                  }
                }
                ;

qnume:          NUME            { name_queue.qualify(@1, $1); }
	|       qnume inof NUME { name_queue.qualify(@3, $3); }
                ;

replacements:   replacement
                {
                  cbl_inspect_t inspect( cbl_refer_t(), $1->opers() );
                  $$ = new ast_inspect_list_t(inspect);
                }
                ;
replacement:    replace_oper
                {
                  $$ = new ast_inspect_t;
                  $$->push_back( cbl_inspect_oper_t($1->bound, $1->replaces) );
                }
        |       replacement replace_oper
                {
                  $$->push_back( cbl_inspect_oper_t($2->bound, $2->replaces)  );
                }
                ;
replace_oper:   CHARACTERS BY alpha_val[replace] insp_mtquals[q]
                {
                  $$ = new ast_inspect_oper_t( cbl_inspect_replace_t(NULL,
                                                                     *$replace,
                                                                     $q->before,
                                                                     $q->after) );
                }
        |       first_leading x_by_ys %prec NAME
                {
                  $$ = $2;
                  $$->bound = static_cast<cbl_inspect_bound_t>($1);
                }
                ;

x_by_ys:        x_by_y
                {
                  $$ = new ast_inspect_oper_t(*$1);
                }
        |       x_by_ys x_by_y
                {
                  $$->replaces.push_back(*$2);
                }
                ;
x_by_y:         alpha_val[matching] BY alpha_val[replace] insp_mtquals[q]
                {
                  $$ = new cbl_inspect_replace_t(*$matching, *$replace,
                                                   $q->before, $q->after);
                }
                ;

insp_mtquals:   %empty     { $$ = new cbl_inspect_match_t; }
        |       insp_quals
                ;
insp_quals:     insp_qual  {
                  $$ = new cbl_inspect_match_t;
                  if( $insp_qual.before ) {
                    $$->before = *$insp_qual.qual;
                  } else {
                    $$->after = *$insp_qual.qual;
                  }
                }
        |       insp_quals insp_qual
                {
                  if( ($$->before.active() && $insp_qual.before) ||
                      ($$->after.active() && !$insp_qual.before) ) {
                    error_msg(@2, "duplicate BEFORE/AFTER phrase");
                    YYERROR;
                  }
                  auto p = $insp_qual.before? &$$->before : &$$->after;
                  *p = *$insp_qual.qual;
                }
                ;
insp_qual:      befter initial alpha_val
                {
                  // NIST NC115A: INITIAL has no effect (GnuCOBOL & ISO say same).
                  bool initial = $initial == INITIAL_kw;
                  $$.before = $befter == BEFORE;
                  $$.qual = new cbl_inspect_qual_t(initial, *$3);
                }
                ;

first_leading:  FIRST      { $$ = bound_first_e; }
        |       ALL        { $$ = bound_all_e; }
        |       LEADING    { $$ = bound_leading_e; }
        |       TRAILING   { $$ = bound_trailing_e;
		  if( ! dialect_mf() ) {
		    dialect_error(@1, "TRAILING", "mf");
		  }
		}
                ;

alphaval:       LITERAL { $$ = new_reference(new_literal($1, quoted_e)); }
        |       reserved_value
                {
                  $$ = new_reference( constant_of(constant_index($1)) );
                }
        |       intrinsic_call
                ;

befter:         BEFORE { $$ = BEFORE; }
        |       AFTER  { $$ = AFTER; }
		;

initialize:     INITIALIZE move_tgts[tgts]
                {
                  statement_begin(@1, INITIALIZE);
                  initialize_statement( $tgts->targets, false, data_category_none );
                }
        |       INITIALIZE move_tgts[tgts] with FILLER_kw
                {
                  statement_begin(@1, INITIALIZE);
                  initialize_statement( $tgts->targets, true, data_category_none );
                }
        |       INITIALIZE move_tgts[tgts] init_clause[ini]
                {
                  statement_begin(@1, INITIALIZE);
		  initialize_statement( $tgts->targets, false, $ini->category,
                                        $ini->replacement);
                }
        |       INITIALIZE move_tgts[tgts] init_clause[ini] with FILLER_kw
                {
                  statement_begin(@1, INITIALIZE);
                  initialize_statement( $tgts->targets, true, $ini->category,
                                        $ini->replacement);
                }
        |       INITIALIZE move_tgts[tgts] with FILLER_kw init_clause[ini]
                {
                  statement_begin(@1, INITIALIZE);
                  initialize_statement( $tgts->targets, true, $ini->category,
                                        $ini->replacement );
                }
               ;

init_clause:    init_value
        |       init_categora
                {
                  $$ = new init_statement_t(false);
                  $$->category = $1;
                }
        |       init_categora to VALUE
                {
                  $$ = new init_statement_t(true);
                  $$->category = $1;
                }
        |       init_categora to VALUE init_value
                {
                  $$ = $init_value;
                  $$->category = $1;
                }
                ;

init_value:     init_replace then to DEFAULT
                {
                  $$ = new init_statement_t( *$init_replace);
                }
        |       init_replace
                {
                  $$ = new init_statement_t( *$init_replace);
                }
        |       then to DEFAULT
                {
                  $$ = new init_statement_t( false );
                }
                ;

init_categora:  init_category
        |       ALL { $$ = data_category_all; }
                ;
init_category:  ALPHABETIC          { $$ = data_alphabetic_e; }
        |       ALPHANUMERIC        { $$ = data_alphanumeric_e; }
        |       ALPHANUMERIC_EDITED { $$ = data_alphanumeric_edited_e; }
        |       DBCS                { $$ = data_dbcs_e; }
        |       EGCS                { $$ = data_egcs_e; }
        |       NATIONAL            { $$ = data_national_e; }
        |       NATIONAL_EDITED     { $$ = data_national_edited_e; }
        |       NUMERIC             { $$ = data_numeric_e; }
        |       NUMERIC_EDITED      { $$ = data_numeric_edited_e; }
                ;

init_replace:   then REPLACING init_bys { $$ = $init_bys; }
                ;
init_bys:       init_by
                {
                  $$ = new category_map_t;
                  category_map_t& replacements = *$$;
                  replacements[$init_by.category] = $init_by.replacement;
                }
        |       init_bys init_by
                {
                  $$ = $1;
                  category_map_t& replacements = *$$;
                  replacements[$init_by.category] = $init_by.replacement;
                }
                ;
init_by:        init_category data BY init_data
                {
                  $$.category = $init_category;
                  $$.replacement = $init_data;
                }
                ;
init_data:      alpha_val
        |       NUMSTR   {
                  $$ = new_reference(new_literal($1.string, $1.radix));
                }
                ;

call:           call_impl end_call
        |       call_cond end_call
                ;

call_impl:      CALL call_body[body]
                {
                  ffi_args_t *params = $body.using_params;
                  if( yydebug && params ) params->dump();
                  size_t narg = params? params->elems.size() : 0;
                  std::vector <cbl_ffi_arg_t> args(narg);
		  cbl_ffi_arg_t *pargs = NULL;
                  if( narg > 0 ) {
		    std::copy( params->elems.begin(),
			       params->elems.end(), args.begin() );	 
                    pargs = args.data();
                  }
                  ast_call( $body.loc, *$body.ffi_name,
                               *$body.ffi_returning, narg, pargs, NULL, NULL, false );
                  current.declaratives_evaluate();
                }
                ;
call_cond:      CALL call_body[body] call_excepts[except]
                {
                  ffi_args_t *params = $body.using_params;
                  if( yydebug && params ) params->dump();
                  size_t narg = params? params->elems.size() : 0;
                  std::vector <cbl_ffi_arg_t> args(narg);
		  cbl_ffi_arg_t *pargs = NULL;
                  if( narg > 0 ) {
		    std::copy( params->elems.begin(),
			       params->elems.end(), args.begin() );	 
                    pargs = args.data();
                  }
                  ast_call( $body.loc, *$body.ffi_name,
                               *$body.ffi_returning, narg, pargs,
                               $except.on_error, $except.not_error, false );
                  auto handled = ec_type_t( static_cast<size_t>(ec_program_e) |
                                            static_cast<size_t>(ec_external_e));
                  current.declaratives_evaluate(handled);
                }
                ;
end_call:       %empty %prec CALL
        |       END_CALL
                ;

call_body:      ffi_name
                { statement_begin(@1, CALL);
                  $$.ffi_name = $ffi_name;
                  $$.using_params = NULL;
                  $$.ffi_returning = cbl_refer_t::empty();
                }

        |       ffi_name USING parameters
                { statement_begin(@1, CALL);
                  $$.ffi_name = $ffi_name;
                  $$.using_params = $parameters;
                  $$.ffi_returning = cbl_refer_t::empty();
                }
        |       ffi_name call_returning scalar[ret]
                { statement_begin(@1, CALL);
                  $$.ffi_name = $ffi_name;
                  $$.using_params = NULL;
                  $$.ffi_returning = $ret;
                }
        |       ffi_name USING parameters call_returning scalar[ret]
                { statement_begin(@1, CALL);
                  $$.ffi_name = $ffi_name;
                  $$.using_params = $parameters;
                  $$.ffi_returning = $ret;
                }
                ;
call_returning:	RETURNING
	|	GIVING {
		  if( !dialect_mf() ) {
		    dialect_error(@1, "CALL ... GIVING", "mf");
		  }
		}
		;

entry:          ENTRY LITERAL
                { statement_begin(@1, ENTRY);
                  auto name = new_literal($2, quoted_e);
                  parser_entry( name );
                }
        |       ENTRY LITERAL USING parameters
                { statement_begin(@1, ENTRY);
                  auto name = new_literal($2, quoted_e);
                  ffi_args_t *params = $parameters;
                  size_t narg = params? params->elems.size() : 0;
		  cbl_ffi_arg_t *pargs = NULL;
                  std::vector <cbl_ffi_arg_t> args(narg);
                  if( narg > 0 ) {
		    std::copy( params->elems.begin(),
			       params->elems.end(), args.begin() );
                    pargs = args.data();
                  }
                  parser_entry( name, narg, pargs );
                }
                ;

ffi_name:       scalar
                {
                  $$ = $1;
                  if( ! is_callable($1->field) ) {
                    error_msg(@1, "CALL requires %s to be "
                             "PROGRAM-POINTER or alphanumeric", $1->name());
                    YYERROR;
                  }
                  if( $1->field->type == FldLiteralA ) {
                    // Replace repository literal with aliased program's name.
                    assert($1->field->parent > 0);
                    auto& L = *cbl_label_of(symbol_at($1->field->parent));
                    $$->field = new_literal(strlen(L.name), L.name, quoted_e);
                  }
                }
        |       LITERAL { $$ = new_reference(new_literal($1, quoted_e)); }
                ;

parameters:     parameter { $$ = new ffi_args_t($1); }
        |       parameters parameter
                {
                  $1->push_back($2);
                  $$ = $1;
                }
                ;
parameter:      ffi_by_ref { $$ = $1; $$->crv = by_default_e; }
        |       by REFERENCE ffi_by_ref { $$ = $3; }
        |       by CONTENT   ffi_by_con { $$ = $3; }
        |       by VALUE     ffi_by_val { $$ = $3; }
                ;
ffi_by_ref:     scalar_arg[refer]
                {
                  $$ = new cbl_ffi_arg_t(by_reference_e, $refer);
                }
        |       ADDRESS OF scalar_arg[refer]
                {
                  $$ = new cbl_ffi_arg_t(by_reference_e, $refer, address_of_e);
                }
        |       OMITTED
                {
                  cbl_refer_t *r = new cbl_refer_t();
                  $$ = new cbl_ffi_arg_t(by_reference_e, r);
                }
                ;

ffi_by_con:     expr
                {
                  cbl_refer_t *r = new cbl_refer_t(*$1);
                  $$ = new cbl_ffi_arg_t(by_content_e, r);
                }
        |       LITERAL
                {
                  cbl_refer_t *r = new_reference(new_literal($1, quoted_e));
                  $$ = new cbl_ffi_arg_t(by_content_e, r);
                }
        |       OMITTED
                {
                  cbl_refer_t *r = new cbl_refer_t();
                  $$ = new cbl_ffi_arg_t(by_content_e, r);
                }
                ;

ffi_by_val:     by_value_arg
                {
                  $$ = new cbl_ffi_arg_t(by_value_e, $1);
                }
        |       cce_expr %prec NAME
                {
                  auto r = new_reference(new_literal(string_of($1)));
                  $$ = new cbl_ffi_arg_t(by_value_e, r);
                }
        |       ADDRESS OF scalar
                {
                  $$ = new cbl_ffi_arg_t(by_value_e, $scalar, address_of_e);
                }
        |       LENGTH_OF scalar
                {
                  $$ = new cbl_ffi_arg_t(by_value_e, $scalar, length_of_e);
                }
                ;

scalar_arg:     scalar
        |       scalar AS FIXED LENGTH %prec NAME
                ;

call_excepts:   call_excepts[a] call_except[b] statements %prec CALL
                {
                  if( $a.on_error && $a.not_error ) {
                    error_msg(@b, "too many ON EXCEPTION clauses");
                    YYERROR;
                  }
                  // "ON" and "NOT ON" could be reversed, but not duplicated.
                  if( $a.on_error && $b.on_error ) {
                    error_msg(@b, "duplicate ON EXCEPTION clauses");
                    YYERROR;
                  }
                  if( $a.not_error && $b.not_error ) {
                    error_msg(@b, "duplicate NOT ON EXCEPTION clauses");
                    YYERROR;
                  }
                  $$ = $a;
                  if( $b.on_error ) {
                    $$.on_error = $b.on_error;
                    assert($a.not_error);
                  } else {
                    $$.not_error = $b.not_error;
                    assert($a.on_error);
                  }
                  assert( $b.on_error || $b.not_error );
                  assert( ! ($b.on_error && $b.not_error) );
                  cbl_label_t *tgt = $a.on_error? $a.on_error : $a.not_error;
                  parser_call_exception_end(tgt);
                }
        |       call_except[a] statements %prec CALL
                {
                  $$ = $a;
                  assert( $a.on_error || $a.not_error );
                  assert( ! ($a.on_error && $a.not_error) );
                  cbl_label_t *tgt = $a.on_error? $a.on_error : $a.not_error;
                  parser_call_exception_end(tgt);
                }
                ;

call_except:    EXCEPTION
                {
                  $$.not_error = NULL;
                  $$.on_error = label_add(LblArith,
                                          uniq_label("call"), yylineno);
                  if( !$$.on_error ) YYERROR;
                  parser_call_exception( $$.on_error );

                  assert( $1 == EXCEPTION || $1 == NOT );
                  if( $1 == NOT ) {
                    std::swap($$.on_error, $$.not_error);
                  }
                }
        |       OVERFLOW_kw
                {
                  $$.not_error = NULL;
                  $$.on_error = label_add(LblArith,
                                          uniq_label("call"), yylineno);
                  if( !$$.on_error ) YYERROR;
                  parser_call_exception( $$.on_error );

                  assert( $1 == OVERFLOW_kw || $1 == NOT );
                  if( $1 == NOT ) {
                    std::swap($$.on_error, $$.not_error);
                  }
                }
                ;

cancel:         CANCEL ffi_names
                {
                  statement_begin(@1, CANCEL);
                  std::vector <cbl_refer_t> progs($ffi_names->refers.size());
		  std::copy( $ffi_names->refers.begin(),
			     $ffi_names->refers.end(), progs.begin() );
                  parser_initialize_programs( progs.size(), progs.empty()? NULL : progs.data() );
                }
                ;
ffi_names:      ffi_name           { $$ = new refer_list_t($1); }
        |       ffi_names ffi_name { $$ = $1->push_back($2); }
                ;

alter:          ALTER { statement_begin(@1, ALTER); } alter_tgts
                ;

alter_tgts:     alter_tgt
        |       alter_tgts alter_tgt
                ;
alter_tgt:      label_1[old] alter_to label_1[new]
                {
                  cbl_perform_tgt_t tgt( $old, $new );
                  parser_alter(&tgt);

                  auto prog = cbl_label_of( symbol_at(symbol_elem_of($old)->program));
                  if( prog->initial ) {
                    cbl_unimplemented("ALTER %s", $old->name);
                  }
                }
                ;

alter_to:       TO
        |       TO PROCEED TO
                ;

go_to:          GOTO labels[args]
                {
                  statement_begin(@1, GOTO);
                  size_t narg = $args->elems.size();
                  if( 1 != narg ) {
                    error_msg(@args, "more than one GO TO label requires DEPENDING");
                    YYERROR;
                  }

		  for( auto& label : $args->elems ) {
		    label->used = yylineno;
		  }
                  cbl_label_t *arg = $args->elems.front();
                  parser_goto( cbl_refer_t(), 1, &arg );
                }
        |       GOTO labels[args] DEPENDING on scalar[value]
                {
                  statement_begin(@1, GOTO);
		  assert(! $args->elems.empty());
                  std::vector <cbl_label_t *> args($args->elems.size());
		  std::copy($args->elems.begin(), $args->elems.end(), args.begin());
		  for( auto& label : $args->elems ) {
		    label->used = yylineno;
		  }
                  parser_goto( *$value, args.size(), args.data() );
                }
        |       GOTO
                {
                  cbl_unimplemented("altered GO TO syntax (format 3)");
                  YYERROR;
                }
                ;

resume:         RESUME NEXT STATEMENT
                {
                  statement_begin(@1, RESUME);
                  parser_clear_exception();
                }
        |       RESUME label_1[tgt]
                {
                  statement_begin(@1, RESUME);
                  parser_clear_exception();
		  $tgt->used = yylineno;
                  parser_goto( cbl_refer_t(), 1, &$tgt );
                }
                ;

labels:         label_1 { $$ = new Label_list_t($1); }
        |       labels label_1 { $$ = $1->push_back($2); }
                ;
label_1:        qname
                { // Add a forward label with no line number, or get an existing.
                  assert(!name_queue.empty());
		  auto namelocs( name_queue.pop() );

		  auto nameloc = namelocs.back();
                  if( namelocs.size() > 2 ) {
                    error_msg(nameloc.loc,
			      "too many qualifications for %s", nameloc.name);
                    YYERROR;
                  }
                  const char *para = nameloc.name;
                  size_t isect = 0;

                  if( namelocs.size() == 2 ) {
		    auto nameloc = namelocs.front();
                    cbl_label_t *sect = label_add(nameloc.loc, LblSection, nameloc.name);
                    isect = symbol_index(symbol_elem_of(sect));
                  }

                  $$ = paragraph_reference(para, isect);
                  assert($$);
                  if( yydebug ) dbgmsg( "using procedure %s of line %d",
                                       $$->name, $$->line );
                }
        |       NUMSTR
                {
                  // Add a forward label with no line number, or get an existing.
                  $$ = label_add(@1, LblNone, $1.string);
                  assert($$ != NULL);
                }
                ;

  /* string & unstring */


string:         string_impl end_string
        |       string_cond end_string
                ;
string_impl:    STRING_kw string_body[body]
                {
                  stringify($body.inputs, *$body.into.first, *$body.into.second);
                  current.declaratives_evaluate(ec_none_e);
                }
                ;
string_cond:    STRING_kw string_body[body] on_overflows[over]
                {
                  stringify($body.inputs, *$body.into.first, *$body.into.second,
                            $over.on_error, $over.not_error);
                  current.declaratives_evaluate(ec_overflow_e);
                }
                ;
end_string:     %empty %prec LITERAL
        |       END_STRING
                ;

string_body:    str_delimiteds[inputs] str_into[into]
                {
                  statement_begin(@$, STRING_kw);
                  $$.inputs = $inputs;
                  $$.into = $into;
                }
                ;

str_delimiteds: str_delimited
                {
                  refer_marked_list_t marked($1.delimiter, $1.input);
                  $$ = new refer_collection_t(marked);
                }
        |       str_delimiteds str_delimited[input]
                {
                  // matching delimiters (or none) adds to the list
                  refer_marked_list_t& marked = $1->lists.back();
                  if( !marked.marker ) {
                    marked.push_on($input.delimiter, $input.input);
                  } else { // start a new list
                    $1->push_back( refer_marked_list_t($input.delimiter,
                                                       $input.input) );
                  }
                }
                ;

str_delimited:  str_input DELIMITED by str_size
                {
                  $$.input = $str_input;
                  $$.delimiter = $str_size;
                }
        |       str_input
                {
                  $$.input = $str_input;
                  $$.delimiter = NULL;
                }
                ;

str_input:      scalar
        |       LITERAL { $$ = new_reference(new_literal($1, quoted_e)); }
        |       reserved_value
                {
                  $$ = new_reference(constant_of(constant_index($1)));
                }
        |       intrinsic_call
                ;

str_size:       SIZE   { $$ = new_reference(NULL); }
        |       LITERAL { $$ = new_reference(new_literal($1, quoted_e)); }
        |       scalar
        |       reserved_value
                {
                  $$ = new_reference(constant_of(constant_index($1)));
                }
                ;

str_into:       INTO scalar
                {
                  $$.first = $2;
                  $$.second = new_reference(NULL);
                }
        |       INTO scalar with POINTER scalar[from]
                {
                  $$.first = $2;
                  $$.second = $from;
                }
                ;

on_overflows:   on_overflow[over] statements %prec ADD
                {
                  assert( $over.on_error || $over.not_error );
                  assert( ! ($over.on_error && $over.not_error) );
                  cbl_label_t *tgt = $over.on_error?
                    $over.on_error : $over.not_error;
                  parser_string_overflow_end(tgt);
                }
        |       on_overflows[a] on_overflow[b] statements %prec ADD
                {
                  if( $a.on_error && $a.not_error ) {
                    error_msg(@b, "too many ON OVERFLOW clauses");
                    YYERROR;
                  }
                  // "ON" and "NOT ON" could be reversed, but not duplicated.
                  if( $a.on_error && $b.on_error ) {
                    error_msg(@b, "duplicate ON OVERFLOW clauses");
                    YYERROR;
                  }
                  if( $a.not_error && $b.not_error ) {
                    error_msg(@b, "duplicate NOT ON OVERFLOW clauses");
                    YYERROR;
                  }
                  $$ = $a;
                  if( $b.on_error ) {
                    $$.on_error = $b.on_error;
                    assert($a.not_error);
                  } else {
                    $$.not_error = $b.not_error;
                    assert($a.on_error);
                  }
                  assert( $b.on_error || $b.not_error );
                  assert( ! ($b.on_error && $b.not_error) );
                  cbl_label_t *tgt = $b.on_error?
                    $b.on_error : $b.not_error;
                  parser_string_overflow_end(tgt);
                }
                ;

on_overflow:    OVERFLOW_kw
                {
                  $$.not_error = NULL;
                  $$.on_error = label_add(LblString,
                                          uniq_label("string"), yylineno);
                  if( !$$.on_error ) YYERROR;
                  parser_string_overflow( $$.on_error );

                  assert( $1 == OVERFLOW_kw || $1 == NOT );
                  if( $1 == NOT ) {
                    std::swap($$.on_error, $$.not_error);
                  }
                }
                ;

unstring:       unstring_impl end_unstring
        |       unstring_cond end_unstring
                ;
end_unstring:   %empty %prec UNSTRING
        |       END_UNSTRING
                ;

unstring_impl:  UNSTRING unstring_body[body]
                {
                  unstringify( *$body.input, $body.delimited, $body.into );
                  current.declaratives_evaluate(ec_none_e);
                }
                ;
unstring_cond:  UNSTRING unstring_body[body] on_overflows[over]
                {
                  unstringify( *$body.input, $body.delimited, $body.into,
                               $over.on_error, $over.not_error );
                  current.declaratives_evaluate(ec_overflow_e);
                }
                ;

unstring_body:  unstring_src[src] uns_delimited INTO uns_into[into]
                {
                  statement_begin(@$, UNSTRING);
                  $$.input = $src;
                  $$.delimited = $uns_delimited;
                  $$.into = $into;
                }
unstring_src:   scalar
        |       intrinsic_call
        |       LITERAL
                {
                  $$ = new_reference(new_literal($1, quoted_e));
                }
                ;

uns_delimited:  %empty { $$ = NULL; }
        |       DELIMITED by uns_delimiters { $$ = $3; }
                ;

uns_delimiters: uns_delimiter { $$ = new refer_list_t($1); }
        |       uns_delimiters OR uns_delimiter
                {
                  $$ = $1;
                  $$->push_back($3);
                }
                ;
uns_delimiter:  all str_input
                {
                  $$ = $2;
                  $$->all = $all;
                }
                ;

uns_into:       uns_tgts %prec NAME
                {
                  $$ = new unstring_into_t($1);
                }
        |       uns_tgts with POINTER scalar[ptr]
                {
                  $$ = new unstring_into_t($1, $ptr);
                }
        |       uns_tgts                          TALLYING in scalar[tally]
                {
                  $$ = new unstring_into_t($1, NULL, $tally);
                }
        |       uns_tgts with POINTER scalar[ptr] TALLYING in scalar[tally]
                {
                  $$ = new unstring_into_t($1, $ptr, $tally);
                }
                ;

uns_tgts:       uns_tgt          { $$ = new unstring_tgt_list_t($1); }
        |       uns_tgts uns_tgt { $$ = $1; $$->push_back($2); }
                ;
uns_tgt:        scalar[tgt]
                {
                  $$ = new unstring_tgt_t($tgt);
                }
        |       scalar[tgt] DELIMITER in scalar[delim]
                {
                  $$ = new unstring_tgt_t($tgt, $delim);
                }
        |       scalar[tgt]                            COUNT in scalar[count]
                {
                  if( ! $count->field->is_integer() ) {
                    error_msg(@count, "COUNT %s must be integer type",
                             $count->field->name);
                  }
                  if( $count->field->has_attr(scaled_e) ) {
                    error_msg(@count, "COUNT %s may not be P scaled",
                             $count->field->name);
                  }
                  $$ = new unstring_tgt_t($tgt, NULL, $count);
                }
        |       scalar[tgt] DELIMITER in scalar[delim] COUNT in scalar[count]
                {
                  if( ! $count->field->is_integer() ) {
                    error_msg(@count, "COUNT %s must be integer type",
                             $count->field->name);
                  }
                  if( $count->field->has_attr(scaled_e) ) {
                    error_msg(@count, "COUNT %s may not be P scaled",
                             $count->field->name);
                  }
                  $$ = new unstring_tgt_t($tgt, $delim, $count);
                }
                ;

  /* intrinsics */
intrinsic_call: function intrinsic { // "intrinsic" includes UDFs.
                  $$ = new_reference($intrinsic);
                  $$->field->attr |= constant_e;
                }
        |       function intrinsic refmod[ref]
                {
                  if( $ref.from->is_reference() || $ref.len->is_reference() ) {
                    error_msg(@ref, "subscripts on start:len refmod "
                            "parameters are unsupported");
                    YYERROR;
                  }
                  if( $intrinsic->type != FldAlphanumeric ) {
                    error_msg(@ref, "'%s' only AlphaNumeric fields accept refmods",
                             $intrinsic->name);
                    YYERROR;
                  }
                  cbl_span_t span( $ref.from, $ref.len );
                  $$ = new cbl_refer_t($intrinsic, span);
                  $$->field->attr |= constant_e;
                }
	|	function NAME {
		  error_msg(@NAME, "no such function: %s", $NAME);
		  YYERROR;
		}

                ;
function:       %empty   %prec FUNCTION
                {
                  statement_begin(@$, FUNCTION);
                }
        |       FUNCTION
                {
                  statement_begin(@1, FUNCTION);
                }
                ;

function_udf:   FUNCTION_UDF '(' arg_list[args] ')' {
		  std::vector<function_descr_arg_t> params;
                  auto L = cbl_label_of(symbol_at($1));
		  if( ! current.udf_args_valid(L, $args->refers, params) ) {
		    YYERROR;
		  }
                  $$ = new_temporary_clone(cbl_field_of(symbol_at(L->returning)));
                  std::vector <cbl_ffi_arg_t> args($args->refers.size());
		  size_t i = 0;
		  // Pass parameters as defined by the function.
                  std::transform( $args->refers.begin(), $args->refers.end(), args.begin(),
				  [params, &i]( cbl_refer_t& arg ) {
				    function_descr_arg_t param = params.at(i++);
				    auto ar = new cbl_refer_t(arg);
				    cbl_ffi_arg_t actual(param.crv, ar);
				    return actual;
				  } );
		  auto name = new_literal(strlen(L->name), L->name, quoted_e);
		  ast_call( @1, name, $$, args.size(), args.data(), NULL, NULL, true );
                }
        |       FUNCTION_UDF_0 {
                  static const size_t narg = 0;
                  static cbl_ffi_arg_t *args = NULL;

                  auto L = cbl_label_of(symbol_at($1));
                  $$ = new_temporary_clone(cbl_field_of(symbol_at(L->returning)));

                  auto name = new_literal(strlen(L->name), L->name, quoted_e);
                  ast_call( @1, name, $$, narg, args, NULL, NULL, true );
                }
                ;

                /*
                 * The scanner returns a function-token (e.g. NUMVAL) if it was
                 * preceded by FUNCTION, or if the name is in the program's
                 * function repository.  Else it returns NAME, because it looks
                 * like a user-defined name (possibly a data item). If the user
                 * attempts to use an intrinsic function without using
                 * REPOSITORY or FUNCTION, the NAME results in a syntax error.
                 *
                 * Function arguments may be variables or literals or
                 * functions, and string-valued functions accept a refmod. In
                 * addition to "scalar", we have this inconsistent set:
                 *  var: [ALL] LITERAL, NUMSTR, instrinsic, or scalar
                 *  num_operand: signed NUMSTR/ZERO, instrinsic, or scalar
                 *  alpahaval: LITERAL, reserved_value, instrinsic, or scalar
                 * Probably any numeric argument could be an expression.
                 */
intrinsic:      function_udf
        |       intrinsic0
        |       intrinsic_v '(' arg_list[args] ')' {
                  location_set(@1);
                  std::vector <cbl_refer_t> args($args->size());
		  assert(! args.empty());
                  std::copy( $args->begin(), $args->end(), args.begin() );
                  cbl_refer_t *p = intrinsic_inconsistent_parameter(args.size(),
								    args.data());
                  if( p != NULL ) {
		    auto loc = symbol_field_location(field_index(p->field));
                    error_msg(loc, "FUNCTION %s has "
                              "inconsistent parameter type %zu ('%s')",
                              keyword_str($1), p - args.data(), name_of(p->field) );
                    YYERROR;
                  }
                  $$ = is_numeric(args[0].field)?
                         new_tempnumeric_float() :
                         new_alphanumeric();

                  parser_intrinsic_callv( $$, intrinsic_cname($1),
					  args.size(), args.data() );
                }

        |       PRESENT_VALUE '(' expr_list[args] ')'
                {
                  static char s[] = "__gg__present_value";
                  location_set(@1);
                  $$ = new_tempnumeric_float();
                  size_t n = $args->size();
                  assert(n > 0);
                  if( n < 2 ) {
                    error_msg(@args, "PRESENT VALUE requires 2 parameters");
                    YYERROR;
                  }
                  std::vector <cbl_refer_t> args(n);
		  std::copy( $args->begin(), $args->end(), args.begin() );
                  parser_intrinsic_callv( $$, s, args.size(), args.data() );
                }

	|       BASECONVERT  '(' varg[r1] varg[r2] varg[r3] ')' {
                  location_set(@1);
                  $$ = new_tempnumeric();
		  cbl_unimplemented("BASECONVERT");
                  if( ! intrinsic_call_3($$, BASECONVERT, $r1, $r2, $r3 )) YYERROR;
                }
        |       BIT_OF  '(' expr[r1] ')' {
                  location_set(@1);
                  $$ = new_alphanumeric();
                  if( ! intrinsic_call_1($$, BIT_OF, $r1, @r1)) YYERROR;
                }
        |       CHAR  '(' expr[r1] ')' {
                  location_set(@1);
                  $$ = new_alphanumeric(1);
                  if( ! intrinsic_call_1($$, CHAR, $r1, @r1)) YYERROR;
                }

	|       CONVERT  '(' varg[r1] convert_src[src] convert_dst[dst] ')' {
                  location_set(@1);
                  $$ = new_alphanumeric(1);
		  cbl_unimplemented("CONVERT");
                  /* if( ! intrinsic_call_3($$, CONVERT, $r1, $src, $dst) ) YYERROR; */
                }

        |       DISPLAY_OF  '(' varg[r1]  ')' {
                  location_set(@1);
                  $$ = new_alphanumeric();
                  if( ! intrinsic_call_2($$, DISPLAY_OF, $r1, NULL) ) YYERROR;
                }
        |       DISPLAY_OF  '(' varg[r1] varg[r2]  ')' {
                  location_set(@1);
                  $$ = new_alphanumeric();
                  if( ! intrinsic_call_2($$, DISPLAY_OF, $r1, $r2) ) YYERROR;
                }

        |       EXCEPTION_FILE filename {
                  location_set(@1);
                  $$ = new_alphanumeric();
                  parser_exception_file( $$, $filename );
                }

        |       FIND_STRING '(' varg[r1] last start_after anycase ')' {
                  location_set(@1);
                  $$ = new_alphanumeric();
                  /* auto r1 = new_reference(new_literal(strlen($r1), $r1, quoted_e)); */
		  cbl_unimplemented("FIND_STRING");
                  /* if( ! intrinsic_call_4($$, FIND_STRING, r1, $r2) ) YYERROR; */
                }

        |       FORMATTED_DATE '(' DATE_FMT[r1] expr[r2] ')' {
                  location_set(@1);
                  $$ = new_alphanumeric(MAXLENGTH_FORMATTED_DATE);
                  auto r1 = new_reference(new_literal(strlen($r1), $r1, quoted_e));
                  if( ! intrinsic_call_2($$, FORMATTED_DATE, r1, $r2) ) YYERROR;
                }


        |       FORMATTED_DATETIME '(' DATETIME_FMT[r1] expr[r2]
                                                        expr[r3] ')' {
                  location_set(@1);
                  $$ = new_alphanumeric(MAXLENGTH_FORMATTED_DATETIME);
                  auto r1 = new_reference(new_literal(strlen($r1), $r1, quoted_e));
                  static cbl_refer_t r3(literally_zero);
                  if( ! intrinsic_call_4($$, FORMATTED_DATETIME,
                                         r1, $r2, $r3, &r3) ) YYERROR;
                }
        |       FORMATTED_DATETIME '(' DATETIME_FMT[r1] expr[r2]
                                        expr[r3] expr[r4] ')' {
                  location_set(@1);
                  $$ = new_alphanumeric(MAXLENGTH_FORMATTED_DATETIME);
                  auto r1 = new_reference(new_literal(strlen($r1), $r1, quoted_e));
                  if( ! intrinsic_call_4($$, FORMATTED_DATETIME,
                                             r1, $r2, $r3, $r4) ) YYERROR;
                }
        |       FORMATTED_DATETIME '(' error ')' {
                  YYERROR;
                }
        |       FORMATTED_TIME '(' TIME_FMT[r1] expr[r2]
                                                expr[r3]  ')' {
                  location_set(@1);
                  $$ = new_alphanumeric(MAXLENGTH_FORMATTED_TIME);
                  auto r1 = new_reference(new_literal(strlen($r1), $r1, quoted_e));
                  if( ! intrinsic_call_3($$, FORMATTED_TIME,
                                             r1, $r2, $r3) ) YYERROR;
                }
        |       FORMATTED_TIME '(' TIME_FMT[r1] expr[r2]  ')' {
                  location_set(@1);
                  $$ = new_alphanumeric(MAXLENGTH_FORMATTED_TIME);
                  auto r3 = new_reference(new_literal("0"));
                  auto r1 = new_reference(new_literal(strlen($r1), $r1, quoted_e));
                  if( ! intrinsic_call_3($$, FORMATTED_TIME,
                                             r1, $r2, r3) ) YYERROR;
                }
        |       FORMATTED_CURRENT_DATE '(' DATETIME_FMT[r1] ')' {
                  location_set(@1);
                  $$ = new_alphanumeric(MAXLENGTH_FORMATTED_DATETIME);
                  auto r1 = new_reference(new_literal(strlen($r1), $r1, quoted_e));
                  if( ! intrinsic_call_1($$, FORMATTED_CURRENT_DATE, r1, @r1) )
                                         YYERROR;
                }
        |       TEST_FORMATTED_DATETIME '(' DATE_FMT[r1] varg[r2] ')' {
                location_set(@1);
                  $$ = new_tempnumeric();
                  auto r1 = new_reference(new_literal(strlen($r1), $r1, quoted_e));
                  if( ! intrinsic_call_2($$, TEST_FORMATTED_DATETIME,
                                              r1, $r2) ) YYERROR;
                }
        |       TEST_FORMATTED_DATETIME '(' TIME_FMT[r1] varg[r2] ')' {
                location_set(@1);
                  $$ = new_tempnumeric();
                  auto r1 = new_reference(new_literal(strlen($r1), $r1, quoted_e));
                  if( ! intrinsic_call_2($$, TEST_FORMATTED_DATETIME,
                                              r1, $r2) ) YYERROR;
                }
        |       TEST_FORMATTED_DATETIME '(' DATETIME_FMT[r1] varg[r2] ')'
                {
                location_set(@1);
                  $$ = new_tempnumeric();
                  auto r1 = new_reference(new_literal(strlen($r1), $r1, quoted_e));
                  if( ! intrinsic_call_2($$, TEST_FORMATTED_DATETIME,
                                              r1, $r2) ) YYERROR;
                }
        |       INTEGER_OF_FORMATTED_DATE '(' DATE_FMT[r1] varg[r2] ')' {
                location_set(@1);
                  $$ = new_tempnumeric();
                  auto r1 = new_reference(new_literal(strlen($r1), $r1, quoted_e));
                  if( ! intrinsic_call_2($$, INTEGER_OF_FORMATTED_DATE,
                                              r1, $r2) ) YYERROR;
                }
        |       INTEGER_OF_FORMATTED_DATE '(' DATETIME_FMT[r1] varg[r2] ')'
                {
                location_set(@1);
                  $$ = new_tempnumeric();
                  auto r1 = new_reference(new_literal(strlen($r1), $r1, quoted_e));
                  if( ! intrinsic_call_2($$, INTEGER_OF_FORMATTED_DATE,
                                              r1, $r2) ) YYERROR;
                }
        |       SECONDS_FROM_FORMATTED_TIME '(' TIME_FMT[r1] varg[r2] ')' {
                location_set(@1);
                  $$ = new_tempnumeric();
                  auto r1 = new_reference(new_literal(strlen($r1), $r1, quoted_e));
                  if( ! intrinsic_call_2($$, SECONDS_FROM_FORMATTED_TIME,
                                              r1, $r2) ) YYERROR;
                }
        |       SECONDS_FROM_FORMATTED_TIME '(' DATETIME_FMT[r1] varg[r2] ')'
                {
                location_set(@1);
                  $$ = new_tempnumeric();
                  auto r1 = new_reference(new_literal(strlen($r1), $r1, quoted_e));
                  if( ! intrinsic_call_2($$, SECONDS_FROM_FORMATTED_TIME,
                                              r1, $r2) ) YYERROR;
                }

        |       HEX_OF  '(' varg[r1] ')' {
                  location_set(@1);
                  $$ = new_alphanumeric();
                  if( ! intrinsic_call_1($$, HEX_OF, $r1, @r1)) YYERROR;
                }
	|	LENGTH '(' tableish[val] ')' {
                  location_set(@1);
                  $$ = new_tempnumeric();
		  $$->clear_attr(signable_e);
		  parser_set_numeric($$, $val->field->size());
		  if( ! intrinsic_call_1($$, LENGTH, $val, @val)) YYERROR;
		}
	|	LENGTH '(' varg1a[val] ')' {
                  location_set(@1);
                  $$ = new_tempnumeric();
		  $$->clear_attr(signable_e);
		  parser_set_numeric($$, $val->field->data.capacity);
		  if( ! intrinsic_call_1($$, LENGTH, $val, @val)) YYERROR;
		}
        |       lopper_case[func] '(' alpha_val[r1] ')' {
                  location_set(@1);
                  $$ = new_alphanumeric($r1->field->data.capacity);
                  if( ! intrinsic_call_1($$, $func, $r1, @r1)) YYERROR;
                }

	|	MODULE_NAME '(' module_type[type] ')'
		{
		  $$ = new_alphanumeric(sizeof(cbl_name_t));
		  parser_module_name( $$, $type );
		}

        |       NUMVAL_C '(' varg[r1] numval_locale[r2] anycase ')' {
                  location_set(@1);
                  $$ = new_tempnumeric();
                  parser_intrinsic_numval_c( $$, *$r1, $r2.is_locale,
                                                      *$r2.arg2, $anycase );
                }
        |       ORD  '(' alpha_val[r1] ')'
                {
                  location_set(@1);
                  $$ = new_tempnumeric();
                  if( ! intrinsic_call_1($$, ORD, $r1, @r1)) YYERROR;
                }
        |       RANDOM
                {
                  location_set(@1);
                  $$ = new_tempnumeric_float();
                  parser_intrinsic_call_0( $$, intrinsic_cname(RANDOM) );
                }
        |       RANDOM_SEED expr[r1] ')'
                { // left parenthesis consumed by lexer
                  location_set(@1);
                  $$ = new_tempnumeric_float();
                  if( ! intrinsic_call_1($$, RANDOM, $r1, @r1)) YYERROR;
                }

        |       STANDARD_COMPARE  '(' varg[r1] varg[r2] varg[r3] varg[r4] ')'
                {
                  location_set(@1);
                  $$ = new_tempnumeric();
		  cbl_unimplemented("STANDARD-COMPARE");
                  /* if( ! intrinsic_call_4($$, STANDARD_COMPARE, $r1) ) YYERROR; */
                }
        |       STANDARD_COMPARE  '(' varg[r1] varg[r2] varg[r3]  ')'
                {
                  location_set(@1);
                  $$ = new_tempnumeric();
		  cbl_unimplemented("STANDARD-COMPARE");
                  /* if( ! intrinsic_call_4($$, STANDARD_COMPARE, $r1) ) YYERROR; */
                }
        |       STANDARD_COMPARE  '(' varg[r1] varg[r2] ')'
                {
                  location_set(@1);
                  $$ = new_tempnumeric();
		  cbl_unimplemented("STANDARD-COMPARE");
                  /* if( ! intrinsic_call_4($$, STANDARD_COMPARE, $r1) ) YYERROR; */
                }

        |       SUBSTITUTE '(' varg[r1] subst_inputs[inputs] ')' {
                  location_set(@1);
                  $$ = new_alphanumeric();
                  std::vector <cbl_substitute_t> args($inputs->size());
                  std::transform( $inputs->begin(), $inputs->end(), args.begin(),
                                  []( const substitution_t& arg ) {
                                    cbl_substitute_t output( arg.anycase,
                                                             char(arg.first_last),
                                                             arg.orig,
                                                             arg.replacement );
                                  return output; } );

                  parser_intrinsic_subst($$, *$r1, args.size(), args.data());
                }


        |       TEST_NUMVAL_C '(' varg[r1] numval_locale[r2] anycase ')' {
                  location_set(@1);
                  $$ = new_tempnumeric();
                  parser_intrinsic_numval_c( $$, *$r1, $r2.is_locale,
                                                 *$r2.arg2, $anycase, true );
                }
        |       TRIM '(' error ')' {
                  error_msg(@error, "invalid TRIM argument");
                  YYERROR;
                }
        |       TRIM '(' expr[r1] trim_trailing ')'
                {
                  location_set(@1);
                   switch( $r1->field->type ) {
                   case FldGroup:
                   case FldAlphanumeric:
                   case FldLiteralA:
                   case FldAlphaEdited:
                   case FldNumericEdited:
                     break; // alphanumeric OK
                   default:
                     // BLANK WHEN ZERO implies numeric-edited, so OK
                     if( $r1->field->has_attr(blank_zero_e) ) {
                       break;
                     }
                     error_msg(@r1, "TRIM argument must be alphanumeric");
                     YYERROR;
                     break;
                  }
                  $$ = new_alphanumeric();
                  cbl_refer_t * how = new_reference($trim_trailing);
                  if( ! intrinsic_call_2($$, TRIM, $r1, how) ) YYERROR;
                }

        |       USUBSTR '(' alpha_val[r1] expr[r2] expr[r3]  ')' {
                  location_set(@1);
                  $$ = new_alphanumeric();
                  if( ! intrinsic_call_3($$, FORMATTED_DATETIME,
                                             $r1, $r2, $r3) ) YYERROR;
                }

        |       intrinsic_I  '(' expr[r1] ')'
                {
                  location_set(@1);
                  $$ = new_tempnumeric();
                  if( ! intrinsic_call_1($$, $1, $r1, @r1)) YYERROR;
                }

        |       intrinsic_N  '(' expr[r1] ')'
                {
                  location_set(@1);
                  $$ = new_tempnumeric_float();
                  if( ! intrinsic_call_1($$, $1, $r1, @r1)) YYERROR;
                }

        |       intrinsic_X  '(' varg[r1] ')'
                {
                  location_set(@1);
                  auto type = intrinsic_return_type($1);
                  switch(type) {
                  case FldAlphanumeric:
                    $$ = new_alphanumeric();
                    break;
                  default:
                    if( $1 == NUMVAL || $1 == NUMVAL_F )
                      {
                      $$ = new_temporary(FldFloat);
                      }
                    else
                      {
                      $$ = new_temporary(type);
                      }
                  }
                  if( $1 == NUMVAL_F ) {
		    if( is_literal($r1->field) && ! is_numeric($r1->field->type) ) {
		      // The parameter might be literal, but could be "hello".
                      auto input = $r1->field->data.initial;
		      error_msg(@r1, "'%s' is not a numeric literal", input);
                    }
                  }
                  if( ! intrinsic_call_1($$, $1, $r1, @r1)) YYERROR;
                }

        |       intrinsic_I2 '(' expr[r1] expr[r2] ')'
                {
                  location_set(@1);
                  $$ = new_tempnumeric();
                  if( ! intrinsic_call_2($$, $1, $r1, $r2) ) YYERROR;
                }

        |       DATE_TO_YYYYMMDD '(' expr[r1] ')'
                {
                  location_set(@1);
                  static auto r2 = new_reference(FldNumericDisplay, "50");
                  static auto one = new cbl_refer_t( new_literal("1") );
                  static auto four = new cbl_refer_t( new_literal("4") );
                  cbl_span_t year(one, four);
                  auto r3 = new_reference(new_alphanumeric(MAXLENGTH_CALENDAR_DATE));
                  r3->refmod = year;

                  parser_intrinsic_call_0( r3->field, "__gg__current_date" );

                  $$ = new_tempnumeric();
                  if( ! intrinsic_call_3($$, DATE_TO_YYYYMMDD,
                                         $r1, r2, r3) ) YYERROR;
                }

        |       DATE_TO_YYYYMMDD '(' expr[r1] expr[r2] ')'
                {
                  location_set(@1);
                  static auto one = new cbl_refer_t( new_literal("1") );
                  static auto four = new cbl_refer_t( new_literal("4") );
                  cbl_span_t year(one, four);
                  auto r3 = new_reference(new_alphanumeric(MAXLENGTH_CALENDAR_DATE));
                  r3->refmod = year;

                  parser_intrinsic_call_0( r3->field, "__gg__current_date" );

                  $$ = new_tempnumeric();
                  if( ! intrinsic_call_3($$, DATE_TO_YYYYMMDD,
                                         $r1, $r2, r3) ) YYERROR;
                }

        |       DATE_TO_YYYYMMDD '(' expr[r1]
                                     expr[r2] expr[r3] ')'
                {
                  location_set(@1);
                  $$ = new_tempnumeric();
                  if( ! intrinsic_call_3($$, DATE_TO_YYYYMMDD,
                                         $r1, $r2, $r3) ) YYERROR;
                }

        |       DAY_TO_YYYYDDD '(' expr[r1] ')'
                {
                  location_set(@1);
                  static auto r2 = new_reference(FldNumericDisplay, "50");
                  static auto one = new cbl_refer_t( new_literal("1") );
                  static auto four = new cbl_refer_t( new_literal("4") );
                  cbl_span_t year(one, four);
                  auto r3 = new_reference(new_alphanumeric(MAXLENGTH_CALENDAR_DATE));
                  r3->refmod = year;

                  parser_intrinsic_call_0( r3->field, "__gg__current_date" );

                  $$ = new_tempnumeric();
                  if( ! intrinsic_call_3($$, DAY_TO_YYYYDDD,
                                         $r1, r2, r3) ) YYERROR;
                }

        |       DAY_TO_YYYYDDD '(' expr[r1] expr[r2] ')'
                {
                  location_set(@1);
                  static auto one = new cbl_refer_t( new_literal("1") );
                  static auto four = new cbl_refer_t( new_literal("4") );
                  cbl_span_t year(one, four);
                  auto r3 = new_reference(new_alphanumeric(MAXLENGTH_CALENDAR_DATE));
                  r3->refmod = year;

                  parser_intrinsic_call_0( r3->field, "__gg__current_date" );

                  $$ = new_tempnumeric();
                  if( ! intrinsic_call_3($$, DAY_TO_YYYYDDD,
                                         $r1, $r2, r3) ) YYERROR;
                }

        |       DAY_TO_YYYYDDD '(' expr[r1]
                                     expr[r2] expr[r3] ')'
                {
                  location_set(@1);
                  $$ = new_tempnumeric();
                  if( ! intrinsic_call_3($$, DAY_TO_YYYYDDD,
                                         $r1, $r2, $r3) ) YYERROR;
                }

        |       YEAR_TO_YYYY '(' expr[r1] ')'
                {
                  location_set(@1);
                  static auto r2 = new_reference(new_literal("50", decimal_e));
                  static auto one = new cbl_refer_t( new_literal("1") );
                  static auto four = new cbl_refer_t( new_literal("4") );
                  cbl_span_t year(one, four);
                  auto r3 = new_reference(new_alphanumeric(MAXLENGTH_CALENDAR_DATE));
                  r3->refmod = year;

                  parser_intrinsic_call_0( r3->field, "__gg__current_date" );

                  $$ = new_tempnumeric();
                  if( ! intrinsic_call_3($$, YEAR_TO_YYYY,
                                         $r1, r2, r3) ) YYERROR;
                }

        |       YEAR_TO_YYYY '(' expr[r1] expr[r2] ')'
                {
                  location_set(@1);
                  static auto one = new cbl_refer_t( new_literal("1") );
                  static auto four = new cbl_refer_t( new_literal("4") );
                  cbl_span_t year(one, four);
                  auto r3 = new_reference(new_alphanumeric(MAXLENGTH_CALENDAR_DATE));
                  r3->refmod = year;

                  parser_intrinsic_call_0( r3->field, "__gg__current_date" );

                  $$ = new_tempnumeric();
                  if( ! intrinsic_call_3($$, YEAR_TO_YYYY,
                                         $r1, $r2, r3) ) YYERROR;
                }

        |       YEAR_TO_YYYY '(' expr[r1]
                                     expr[r2] expr[r3] ')'
                {
                  location_set(@1);
                  $$ = new_tempnumeric();
                  if( ! intrinsic_call_3($$, YEAR_TO_YYYY,
                                         $r1, $r2, $r3) ) YYERROR;
                }

        |       intrinsic_N2 '(' expr[r1] expr[r2] ')'
                {
                  location_set(@1);
                  switch($1)
                    {
                    case ANNUITY:
                      $$ = new_tempnumeric_float();
                      break;
                    case COMBINED_DATETIME:
                      $$ = new_tempnumeric();
                      break;
                    case REM:
                      $$ = new_tempnumeric_float();
                      break;
                    }
                  if( ! intrinsic_call_2($$, $1, $r1, $r2) ) YYERROR;
                }

        |       intrinsic_X2 '(' varg[r1] varg[r2] ')'
                {
                  location_set(@1);
                  $$ = new_alphanumeric();
                  if( ! intrinsic_call_2($$, $1, $r1, $r2) ) YYERROR;
                }
        |       intrinsic_locale
                ;

module_type:	ACTIVATING { $$ = module_activating_e; }
	|	CURRENT	   { $$ = module_current_e; }
	|	NESTED	   { $$ = module_nested_e; }
	|	STACK	   { $$ = module_stack_e; }
	|	TOP_LEVEL  { $$ = module_toplevel_e; }
		;

convert_src:	ANY
	|	HEX
	|	convert_fmt
		;
convert_dst:	convert_fmt HEX
	|	BYTE
		;
convert_fmt:	ALPHANUMERIC
	|	ANUM
	|	NAT
	|	NATIONAL
		;

numval_locale:  %empty {
                  $$.is_locale = false;
                  $$.arg2 = cbl_refer_t::empty();
                }
        |       LOCALE NAME  { $$.is_locale = true;  $$.arg2 = NULL;
                  cbl_unimplemented("NUMVAL_C LOCALE"); YYERROR;
                }
        |       varg         { $$.is_locale = false; $$.arg2 = $1; }
                ;

subst_inputs:   subst_input { $$ = new substitutions_t; $$->push_back($1); }
        |       subst_inputs subst_input { $$ = $1; $$->push_back($2); }
                ;
subst_input:    anycase first_last varg[v1] varg[v2] {
                  $$.init( $anycase, $first_last, $v1, $v2 );
                }
                ;

intrinsic_locale:
                LOCALE_COMPARE '(' varg[r1] varg[r2]  ')'
                {
                  location_set(@1);
                  $$ = new_alphanumeric();
                  cbl_refer_t dummy = {};
                  if( ! intrinsic_call_3($$, LOCALE_COMPARE, $r1, $r2, &dummy) ) YYERROR;
                }
        |       LOCALE_COMPARE '(' varg[r1] varg[r2] varg[r3] ')'
                {
                  location_set(@1);
                  $$ = new_alphanumeric();
                  if( ! intrinsic_call_3($$, LOCALE_COMPARE, $r1, $r2, $r3) ) YYERROR;
                }

        |       LOCALE_DATE '(' varg[r1]  ')'
                {
                  location_set(@1);
                  $$ = new_alphanumeric();
                  cbl_refer_t dummy = {};
                  if( ! intrinsic_call_2($$, LOCALE_DATE, $r1, &dummy) ) YYERROR;
                }
        |             LOCALE_DATE '(' varg[r1] varg[r2]  ')'
                {
                  location_set(@1);
                  $$ = new_alphanumeric();
                  if( ! intrinsic_call_2($$, LOCALE_DATE, $r1, $r2) ) YYERROR;
                }
        |       LOCALE_TIME '(' varg[r1]  ')'
                {
                  location_set(@1);
                  $$ = new_alphanumeric();
                  cbl_refer_t dummy = {};
                  if( ! intrinsic_call_2($$, LOCALE_TIME, $r1, &dummy) ) YYERROR;
                }
        |       LOCALE_TIME '(' varg[r1] varg[r2]  ')'
                {
                  location_set(@1);
                  $$ = new_alphanumeric();
                  if( ! intrinsic_call_2($$, LOCALE_TIME, $r1, $r2) ) YYERROR;
                }
        |       LOCALE_TIME_FROM_SECONDS '(' varg[r1]  ')'
                {
                  location_set(@1);
                  $$ = new_alphanumeric();
                  cbl_refer_t dummy = {};
                  if( ! intrinsic_call_2($$, LOCALE_TIME_FROM_SECONDS, $r1, &dummy) ) YYERROR;
                }
        |       LOCALE_TIME_FROM_SECONDS '(' varg[r1] varg[r2]  ')'
                {
                  location_set(@1);
                  $$ = new_alphanumeric();
                  if( ! intrinsic_call_2($$, LOCALE_TIME_FROM_SECONDS, $r1, $r2) ) YYERROR;
                }
                ;

lopper_case:    LOWER_CASE      { $$ = LOWER_CASE; }
        |       UPPER_CASE      { $$ = UPPER_CASE; }
                ;

trim_trailing:  %empty          { $$ = new_literal("0"); }  // Remove both
        |       LEADING         { $$ = new_literal("1"); }  // Remove leading  spaces
        |       TRAILING        { $$ = new_literal("2"); }  // Remove trailing spaces
        ;

intrinsic0:     CURRENT_DATE {
                  location_set(@1);
                  $$ = new_alphanumeric(MAXLENGTH_CALENDAR_DATE);
                  parser_intrinsic_call_0( $$, "__gg__current_date" );
                }
        |       E {
                  location_set(@1);
                  $$ = new_tempnumeric();
                 parser_intrinsic_call_0( $$, "__gg__e" );
                }

        |       EXCEPTION_FILE_N {
                  location_set(@1);
                  $$ = new_alphanumeric();
                  intrinsic_call_0( $$, EXCEPTION_FILE_N );
                }

        |       EXCEPTION_FILE {
                  location_set(@1);
                  $$ = new_alphanumeric();
                  parser_exception_file( $$ );
                }
        |       EXCEPTION_LOCATION_N {
                  location_set(@1);
                  $$ = new_alphanumeric();
                  intrinsic_call_0( $$, EXCEPTION_LOCATION_N );
                }
        |       EXCEPTION_LOCATION {
                  location_set(@1);
                  $$ = new_alphanumeric();
                  intrinsic_call_0( $$, EXCEPTION_LOCATION );
                }
        |       EXCEPTION_STATEMENT {
                  location_set(@1);
                  $$ = new_alphanumeric();
                  intrinsic_call_0( $$, EXCEPTION_STATEMENT );
                }
        |       EXCEPTION_STATUS {
                  location_set(@1);
                  $$ = new_alphanumeric();
                  intrinsic_call_0( $$, EXCEPTION_STATUS );
                }

        |       PI {
                  location_set(@1);
                  $$ = new_tempnumeric_float();
                 parser_intrinsic_call_0( $$, "__gg__pi" );
                }
        |       SECONDS_PAST_MIDNIGHT {
                  location_set(@1);
                  $$ = new_tempnumeric();
                 intrinsic_call_0( $$, SECONDS_PAST_MIDNIGHT );
                }
        |       UUID4 {
                  location_set(@1);
                  $$ = new_alphanumeric();
                 parser_intrinsic_call_0( $$, "__gg__uuid4" );
                }
        |       WHEN_COMPILED {
                  location_set(@1);
                  $$ = new_alphanumeric(MAXLENGTH_CALENDAR_DATE); // Returns YYYYMMDDhhmmssss-0500
                 parser_intrinsic_call_0( $$, "__gg__when_compiled" );
                }
                ;

intrinsic_I:    BOOLEAN_OF_INTEGER     { $$ = BOOLEAN_OF_INTEGER;
		  cbl_unimplemented("BOOLEAN-OF-INTEGER");
		}
        |       CHAR_NATIONAL          { $$ = CHAR_NATIONAL;
		    cbl_unimplemented("CHAR-NATIONAL");
		}
        |       DATE_OF_INTEGER        { $$ = DATE_OF_INTEGER; }
        |       DAY_OF_INTEGER         { $$ = DAY_OF_INTEGER; }
        |       FACTORIAL              { $$ = FACTORIAL; }
        |       FRACTION_PART          { $$ = FRACTION_PART; }
        |       HIGHEST_ALGEBRAIC      { $$ = HIGHEST_ALGEBRAIC; }
        |       INTEGER                { $$ = INTEGER; }
        |       INTEGER_OF_BOOLEAN     { $$ = INTEGER_OF_BOOLEAN;
		    cbl_unimplemented("INTEGER-OF-BOOLEAN");
		}
        |       INTEGER_OF_DATE        { $$ = INTEGER_OF_DATE; }
        |       INTEGER_OF_DAY         { $$ = INTEGER_OF_DAY; }
        |       INTEGER_PART           { $$ = INTEGER_PART; }
        |       LOWEST_ALGEBRAIC       { $$ = LOWEST_ALGEBRAIC; }
        |       SIGN                   { $$ = SIGN; }
        |       TEST_DATE_YYYYMMDD     { $$ = TEST_DATE_YYYYMMDD; }
        |       TEST_DAY_YYYYDDD       { $$ = TEST_DAY_YYYYDDD; }
        |       ULENGTH                { $$ = ULENGTH; }
        |       UPOS                   { $$ = UPOS; }
        |       USUPPLEMENTARY         { $$ = USUPPLEMENTARY; }
        |       UVALID                 { $$ = UVALID; }
        |       UWIDTH                 { $$ = UWIDTH; }
                ;

intrinsic_I2:   MOD                    { $$ = MOD; }
                ;

intrinsic_N:    ABS                    { $$ = ABS; }
        |       ACOS                   { $$ = ACOS; }
        |       ASIN                   { $$ = ASIN; }
        |       ATAN                   { $$ = ATAN; }
        |       COS                    { $$ = COS; }
        |       EXP                    { $$ = EXP; }
        |       EXP10                  { $$ = EXP10; }
        |       LOG                    { $$ = LOG; }
        |       LOG10                  { $$ = LOG10; }
        |       SIN                    { $$ = SIN; }
        |       SMALLEST_ALGEBRAIC     { $$ = SMALLEST_ALGEBRAIC;
		    cbl_unimplemented("SMALLEST-ALGEBRAIC");
		}
        |       SQRT                   { $$ = SQRT; }
        |       TAN                    { $$ = TAN; }
                ;

intrinsic_N2:   ANNUITY                { $$ = ANNUITY; }
        |       COMBINED_DATETIME      { $$ = COMBINED_DATETIME; }
        |       REM                    { $$ = REM; }
                ;

intrinsic_X:    BIT_TO_CHAR            { $$ = BIT_TO_CHAR; }
        |       BYTE_LENGTH            { $$ = BYTE_LENGTH; }
        |       HEX_TO_CHAR            { $$ = HEX_TO_CHAR; }
        |       NUMVAL                 { $$ = NUMVAL; }
        |       NUMVAL_F               { $$ = NUMVAL_F; }
        |       REVERSE                { $$ = REVERSE; }
        |       TEST_NUMVAL            { $$ = TEST_NUMVAL; }
        |       TEST_NUMVAL_F          { $$ = TEST_NUMVAL_F; }
                ;

intrinsic_X2:   NATIONAL_OF            { $$ = NATIONAL_OF; }
                ;

intrinsic_v:    CONCAT                 { $$ = CONCAT; }
        |       MAXX                   { $$ = MAXX; }
        |       MEAN                   { $$ = MEAN; }
        |       MEDIAN                 { $$ = MEDIAN; }
        |       MIDRANGE               { $$ = MIDRANGE; }
        |       MINN                   { $$ = MINN; }
        |       ORD_MAX                { $$ = ORD_MAX; }
        |       ORD_MIN                { $$ = ORD_MIN; }
        |       RANGE                  { $$ = RANGE; }
        |       STANDARD_DEVIATION     { $$ = STANDARD_DEVIATION; }
        |       SUM                    { $$ = SUM; }
        |       VARIANCE               { $$ = VARIANCE; }
                ;

all:            %empty { $$ = false; }
        |       ALL    { $$ = true; }
                ;

anycase:        %empty  { $$ = false; }
        |       ANYCASE { $$ = true; }
                ;

as:             %empty
        |       AS
                ;

at:             %empty
        |       AT
                ;

by:             %empty
        |       BY
                ;

characters:     %empty
        |       CHARACTERS
                ;

collating:      %empty
        |       COLLATING
                ;

contains:       %empty
        |       CONTAINS
                ;

in:             %empty
        |       IN
                ;

data:           %empty
        |       DATA
                ;

exception:	%empty
        |       EXCEPTION
                ;

file:           %empty
        |       FILE_KW
                ;

first_last:     %empty  { $$ = 0; }
        |       FIRST   { $$ = 'F'; }
        |       LAST    { $$ = 'L'; }
                ;

is_global:      %empty %prec GLOBAL { $$ = false; }
        |       is GLOBAL           { $$ = true; }
                ;

global:         %empty %prec GLOBAL { $$ = false; }
        |       GLOBAL              { $$ = true; }
                ;

initial:        %empty      { $$ = 0; }
        |       INITIAL_kw  { $$ = INITIAL_kw; }
                ;

is:             %empty
        |       IS
                ;

key:            %empty
        |       KEY
                ;

last:		%empty %prec LAST
	|	LAST
		;

lines:          %empty
        |       LINE
        |       LINES
                ;

mode:           %empty
        |       MODE
                ;

native:         %empty
        |       NATIVE
                ;

of:             %empty
        |       OF
                ;

on:             %empty
        |       ON
                ;

optional:       %empty   { $$ = false; }
        |       OPTIONAL { $$ = true;  }
                ;

program_kw:     %empty
        |       PROGRAM_kw
                ;

order:          %empty
        |       ORDER
                ;

record:         %empty
        |       RECORD
                ;

sign:           %empty
        |       SIGN
                ;

start_after:	%empty %prec AFTER
	|	START AFTER varg
		;

status:         %empty
        |       STATUS
                ;
strong:         %empty { $$ = true; }
        |       STRONG { $$ = false; }
                ;

times:          %empty
        |       TIMES
                ;
then:           %empty
        |       THEN
                ;

to:             %empty
        |       TO
                ;

usage:          %empty
        |       USAGE
        |       USAGE IS
                ;

with:           %empty
        |       WITH
                ;

                /*
                 * CDF: Compiler-directing Facility
                 */
cdf:            cdf_none
        |       cdf_library
        |       cdf_listing
        |       cdf_option
                ;

cdf_library:    cdf_basis
        /* |    DELETE */
        |       INSERTT
                ;
cdf_basis:      BASIS NAME /* BASIS is never passed to the parser.  */
        |       BASIS LITERAL
                ;

cdf_use:        USE DEBUGGING on labels
                {
                  if( ! current.declarative_section_name() ) {
                    error_msg(@1, "USE valid only in DECLARATIVES");
                    YYERROR;
                  }
                  std::for_each($labels->elems.begin(), $labels->elems.end(),
                                add_debugging_declarative);

                }
        |       USE DEBUGGING on ALL PROCEDURES
                {
                  if( ! current.declarative_section_name() ) {
                    error_msg(@1, "USE valid only in DECLARATIVES");
                    YYERROR;
                  }
                  static const cbl_label_t all = {
		    LblNone, 0, 0,0,0, false, false, false, 0,0, ":all:" };
		      ////.name = { ':', 'a', 'l', 'l', ':',  } // workaround for gcc < 11.3
                  add_debugging_declarative(&all);
                 }

        |       USE globally mistake procedure on filenames
		{
                  if( ! current.declarative_section_name() ) {
		    error_msg(@1, "USE valid only in DECLARATIVES");
                    YYERROR;
                  }
                  bool global = $globally == GLOBAL;
                  std::list<size_t> files;
		  auto& culprits = $filenames->files;
                    std::transform( culprits.begin(), culprits.end(),
                                    std::back_inserter(files),
                                    file_list_t::symbol_index );
                  cbl_declarative_t declarative(current.declarative_section(),
                                                ec_all_e, files,
                                                file_mode_none_e, global);
                  current.declaratives.add(declarative);
		}

        |       USE globally mistake procedure on io_mode
                { // Format 1
                  if( ! current.declarative_section_name() ) {
                    error_msg(@1, "USE valid only in DECLARATIVES");
                    YYERROR;
                  }
                  bool global = $globally == GLOBAL;
                  std::list<size_t> files;
                  cbl_declarative_t declarative(current.declarative_section(),
                                                ec_all_e, files,
                                                $io_mode, global);
                  current.declaratives.add(declarative);
                }
        |       USE cdf_use_excepts // Format 3: AFTER swallowed by lexer
                {
                  if( ! current.declarative_section_name() ) {
                    error_msg(@1, "USE valid only in DECLARATIVES");
                    YYERROR;
                  }
                }
                ;

cdf_use_excepts:
                cdf_use_except
        |       cdf_use_excepts cdf_use_except
                ;
cdf_use_except: EC NAME cdf_use_files[files]
                {
                  auto ec = ec_type_of($NAME);
                  if( ec == ec_none_e ) {
                    error_msg(@NAME, "not an EXCEPTION CONDITION: %s", $NAME);
                    YYERROR;
                  }
                  std::list<size_t> files;
                  if( $files ) {
                    if( ec_io_e != (ec_io_e & ec) ) {
                      error_msg(@NAME, "not an I-O EXCEPTION CONDITION: %s", $NAME);
                      YYERROR;
                    }
                    auto& culprits = $files->files;
                    std::transform( culprits.begin(), culprits.end(),
                                    std::back_inserter(files),
                                    file_list_t::symbol_index );
                  }

                  cbl_declarative_t declarative(current.declarative_section(),
                                                ec, files, file_mode_none_e);
                  // Check for duplicates, but keep going.
                  current.declaratives.add(declarative);
                }
                ;
cdf_use_files:  %empty            { $$ = NULL; }
        |       FILE_KW filenames { $$ = $2; }
                ;

io_mode:	INPUT	   { $$ = file_mode_input_e; }
        |       OUTPUT     { $$ = file_mode_output_e; }
        |       IO         { $$ = file_mode_io_e; }
        |       EXTEND     { $$ = file_mode_extend_e; }
                ;

globally:       global                            { $$ = $1? GLOBAL : 0; }
        |       global       STANDARD             { $$ = $1? GLOBAL : STANDARD; }
        |       global AFTER                      { $$ = $1? GLOBAL : 0; }
        |       global AFTER STANDARD             { $$ = $1? GLOBAL : STANDARD; }
                ;
mistake:        EXCEPTION           { $$ = EXCEPTION; }
        |       ERROR               { $$ = ERROR; }
                ;
procedure:      %empty
        |       PROCEDURE
                ;

cdf_listing:    STAR_CBL star_cbl_opts
                ;
star_cbl_opts:  star_cbl_opt
        |       star_cbl_opts star_cbl_opt
                ;
star_cbl_opt:   LIST   { $$ = $LIST[0]   == 'N'? NOLIST : LIST; }
        |       MAP    { $$ = $MAP[0]    == 'N'? NOMAP : MAP; }
        /* |    SOURCE { $$ = $SOURCE[0] == 'N'? NOSOURCE : SOURCE; } */
                ;

cdf_option:     CBL cbl_options
                ;
cbl_options:    cbl_option
        |       cbl_options cbl_option
                ;
cbl_option:     LITERAL
                ; /* Ignore all options.  */

                /* The following compiler directing statements have no effect */
cdf_none:       ENTER
        |       READY
        |       RESET
        |       TRACE
        |       SERVICE_RELOAD
        ;


%%

static YYLTYPE
first_line_of( YYLTYPE loc ) {
    if( loc.first_line < loc.last_line ) loc.last_line = loc.first_line;
    if( loc.last_column < loc.first_column ) loc.last_column = loc.first_column;
    return loc;
}

void ast_call( const YYLTYPE& loc, cbl_refer_t name, cbl_refer_t returning,
                  size_t narg, cbl_ffi_arg_t args[],
                  cbl_label_t *except,
                  cbl_label_t *not_except,
                  bool is_function)
{
  if( is_literal(name.field) ) {
    cbl_field_t called = {      0, FldLiteralA, FldInvalid, quoted_e | constant_e,
                                0, 0, 77, nonarray, 0, "",
                                0, cbl_field_t::linkage_t(), {}, NULL };
    snprintf(called.name, sizeof(called.name), "_%s", name.field->data.initial);
    called.data = name.field->data;
    name.field = cbl_field_of(symbol_field_add(PROGRAM, &called));
    symbol_field_location(field_index(name.field), loc);
    parser_symbol_add(name.field);
  }

  parser_call( name, returning, narg, args, except, not_except, is_function );
}

static size_t
statement_begin( const YYLTYPE& loc, int token ) {
  // The following statement generates a message at run-time
  // parser_print_string("statement_begin()\n");
  location_set(loc);
  prior_statement = token;

  parser_statement_begin();

  if( token != CONTINUE ) {
    if( enabled_exceptions.size() ) {
      current.declaratives_evaluate(ec_none_e);
      cbl_enabled_exceptions_array_t enabled(enabled_exceptions);
      parser_exception_prepare( keyword_str(token), &enabled );
    }
  }
  return 0;
}

#include "parse_util.h"
#include <sys/types.h>

struct string_match {
  const char *name;
  string_match( const char name[] ) : name(name) {}
  bool operator()( const char input[] ) const {
    return strlen(name) == strlen(input) && 0 == strcasecmp(name, input);
  }
};

const char *
keyword_str( int token ) {
  if( token == YYEOF )   return "YYEOF";
  if( token == YYEMPTY ) return "YYEMPTY";

  if( token < 256 ) {
    static char ascii[2];
    ascii[0] = token;
    return ascii;
  }

  return tokens.name_of(token);
}

/*
 * Return the token for the Cobol name, unless it is a function name.  The
 * lexer uses keyword_tok to determine if what appears to be a NAME is in fact
 * a token defined by the parser.  For functions, the situation is unambiguous:
 * a function name appears only after FUNCTION or in the REPOSITORY paragraph.
 * All function names are rejected here; the lexer uses typed_name to check
 * REPOSITORY names.
 */

// tokens.h is generated as needed from parse.h with tokens.h.gen
tokenset_t::tokenset_t() {
#include "token_names.h"
}

// Look up the lowercase form of a keyword, excluding some CDF names.
int
tokenset_t::find( const cbl_name_t name, bool include_intrinsics ) {
  static const cbl_name_t non_names[] = { // including CDF NAMES, and "SWITCH"
    "CHECKING", "LIST", "LOCATION", "MAP", "SWITCH",
  }, * const eonames = non_names + COUNT_OF(non_names);

  if( std::any_of(non_names, eonames,
		  [candidate=name](const cbl_name_t non_name) {
		    return 0 == strcasecmp(non_name, candidate)
		      && strlen(non_name) == strlen(candidate);
		  } ) ) {
    return 0; // CDF names are never ordinary tokens
  }

  if( dialect_ibm() ) {
      static const cbl_name_t ibm_non_names[] = {
	  "RESUME",
      }, * const eonames = ibm_non_names + COUNT_OF(ibm_non_names);

      if( std::any_of(ibm_non_names, eonames,
		      [candidate=name](const cbl_name_t non_name) {
			  return 0 == strcasecmp(non_name, candidate)
			      && strlen(non_name) == strlen(candidate);
		      } ) ) {
	  return 0; // Names not reserved by IBM are never ordinary IBM tokens
      }
  }

  cbl_name_t lname;
  std::transform(name, name + strlen(name) + 1, lname, tolower);
  auto p = tokens.find(lname);
  if( p == tokens.end() ) return 0;
  int token = p->second;

  if( token == SECTION ) yylval.number = 0;

  if( include_intrinsics ) return token;

  return intrinsic_cname(token)? 0 : token;
}

int
keyword_tok( const char * text, bool include_intrinsics ) {
  return tokens.find(text, include_intrinsics);
}

static inline size_t
verify_figconst( enum cbl_figconst_t figconst , size_t pos ) {
  cbl_field_t *f = cbl_field_of(symbol_at(pos));
  assert((f->attr & FIGCONST_MASK) == figconst);
  return pos;
}

static size_t
constant_index( int token ) {
  switch(token) {
  case SPACES      : return 0;
  case LOW_VALUES  : return verify_figconst(low_value_e, 2);
  case ZERO        : return verify_figconst(zero_value_e, 3);
  case HIGH_VALUES : return verify_figconst(high_value_e, 4);
  case QUOTES      : return 5;
  case NULLS       : return 6;
  }
  cbl_errx( "%s:%d: no such constant %d", __func__, __LINE__, token);
  return (size_t)-1;
}


static enum relop_t
relop_of(int token) {
  switch(token) {
  case '<': return lt_op;
  case LE:  return le_op;
  case '=': return eq_op;
  case NE:  return ne_op;
  case GE:  return ge_op;
  case '>': return gt_op;
  }
  cbl_internal_error( "%s:%d: invalid relop token %d",
		      __func__, __LINE__, token);

  return lt_op; // not reached
}

static relop_t
relop_invert(relop_t op) {
  switch(op) {
  case lt_op: return ge_op;
  case le_op: return gt_op;
  case eq_op: return ne_op;
  case ne_op: return eq_op;
  case ge_op: return lt_op;
  case gt_op: return le_op;
  }
  cbl_errx( "%s:%d: invalid relop_t %d", __func__, __LINE__, op);

  return relop_t(0); // not reached
}

#if needed
static const char *
relop_debug_str(int token) {
  switch(token) {
  case 0:   return "zilch";
  case '<': return "<";
  case LE:  return "LE";
  case '=': return "=";
  case NE:  return "NE";
  case GE:  return "GE";
  case '>': return ">";
  }
  dbgmsg("%s:%d: invalid relop token %d", __func__, __LINE__, token);
  return "???";
}

static int
token_of(enum relop_t op) {
  switch(op) {
  case lt_op: return '<';
  case le_op: return LE;
  case eq_op: return '=';
  case ne_op: return NE;
  case ge_op: return GE;
  case gt_op: return '>';
  }
  cbl_errx( "%s:%d: invalid relop_t %d", __func__, __LINE__, op);

  return 0; // not reached
}
#endif

static enum classify_t
classify_of( int token ) {
  switch(token) {
  case NUMERIC:             return ClassNumericType;
  case ALPHABETIC:          return ClassAlphabeticType;
  case ALPHABETIC_LOWER:    return ClassLowerType;
  case ALPHABETIC_UPPER:    return ClassUpperType;
  case DBCS:                return ClassDbcsType;
  case KANJI:               return ClassKanjiType;
  }
  return (enum classify_t)-1;
}

static cbl_round_t
rounded_of( int token ) {
  cbl_round_t mode = current_rounded_mode();
  if( 0 <= token && token <= int(truncation_e) ) {
    return cbl_round_t(token);
  }
  switch(token) {
  case ROUNDED:
    mode = current.rounded_mode();
    break;
  case AWAY_FROM_ZERO:
    mode = away_from_zero_e;
    break;
  case NEAREST_TOWARD_ZERO:
    mode = nearest_toward_zero_e;
    break;
  case TOWARD_GREATER:
    mode = toward_greater_e;
    break;
  case TOWARD_LESSER:
    mode = toward_lesser_e;
    break;
  case NEAREST_AWAY_FROM_ZERO:
    mode = nearest_away_from_zero_e;
    break;
  case NEAREST_EVEN:
    mode = nearest_even_e;
    break;
  case PROHIBITED:
    mode = prohibited_e;
    break;
  case TRUNCATION:
    mode = truncation_e;
    break;
  default:
    dbgmsg("%s: logic error: unrecognized rounding value %d", __func__, token);
  }
  return mode;
}

static cbl_round_t
current_rounded_mode( int token ) {
  cbl_round_t mode = rounded_of(token);
  return current.rounded_mode(mode);
}

template <cbl_label_type_t T>
class label_named {
  size_t program;
  const char *name;
 public:
 label_named( size_t program, const char name[] )
   : program(program), name(name) {}
    bool operator()( const symbol_elem_t& sym ) const {
    if( sym.program == program && sym.type == SymLabel ) {
      auto p = cbl_label_of(&sym);
      return p->type == T && 0 == strcasecmp(p->name, name);
    }
    return false;
  }
};

typedef label_named<LblSection> section_named;
typedef label_named<LblParagraph> paragraph_named;

static struct cbl_label_t *
label_add( const YYLTYPE& loc,
	   enum cbl_label_type_t type, const char name[] ) {
  size_t parent = 0;

  // Verify the new paragraph doesn't conflict with a section
  if( type == LblParagraph ) {
    parent = current.program_section();
    auto p = std::find_if(symbols_begin(PROGRAM), symbols_end(),
                          section_named(PROGRAM, name));
    if( p != symbols_end() ) {
      error_msg(loc, "paragraph %s conflicts with section %s on line %d",
                name, cbl_label_of(p)->name, cbl_label_of(p)->line);
    }
  }

  // Verify the new section doesn't conflict with a paragraph
  if( type == LblSection ) {
    // line is zero if the forward reference is to PARA OF SECT
    auto p = std::find_if(symbols_begin(PROGRAM), symbols_end(),
                          paragraph_named(PROGRAM, name));
    if( p != symbols_end() ) {
      error_msg(loc, "section %s conflicts with paragraph %s on line %d",
                name, cbl_label_of(p)->name, cbl_label_of(p)->line);
    }
  }
  struct cbl_label_t label = { type, parent, loc.last_line };

  if( !namcpy(loc, label.name, name) ) return NULL;
  auto p =  symbol_label_add(PROGRAM, &label);

  if( type == LblParagraph || type == LblSection ) {
    procedure_definition_add(PROGRAM, p);
  }

  assert( !(p->type == LblSection && p->parent > 0) );

  return p;
}

/*
 * Many label names are defined statically and so are guaranteed to be in
 * bounds. Often they are created far away from the yacc metavariables, so
 * there's no location to access.
 */
static struct cbl_label_t *
label_add( enum cbl_label_type_t type, const char name[], int line ) {
  YYLTYPE loc { line, 1, line, 1 };
  return label_add(loc, type, name);
}

cbl_label_t *
perform_t::ec_labels_t::new_label( cbl_label_type_t type,
				   const cbl_name_t role )
{
  size_t n = 1 + symbols_end() - symbols_begin();
  cbl_name_t name;
  sprintf(name, "_perf_%s_%zu", role, n);
  return label_add( type, name, yylineno );
}

/*
 * An unqualified procedure reference occurs within a section may refer to a:
 *   1.  section
 *   2.  paragraph, perhaps in a section, perhaps the current section.
 *
 * The named procedure need only be unique, either within the current section
 * or globally. A paragraph within one section may be referenced without
 * qualification in another section if its name is unique.
 *
 * An otherwise globally unique name is shadowed by the same name in the
 * current section, and the section-local name may be referenced before being
 * defined.  That is, given:
 *
 *   S1 SECTION.
 *     PROC.
 *       ...
 *   S2 SECTION.
 *     PERFORM PROC.
 *     PROC. ...
 *
 * the procedure performed is PROC OF S2.
 *
 * That creates a challenge for the compiler, because PROC appears to have been
 * defined when PERFORM is encountered.  When PROC OF S2 is defined, the parser
 * detects and corrects its misstep.
 */
static struct cbl_label_t *
paragraph_reference( const char name[], size_t section )
{
  // A reference has line == 0.  It is LblParagraph if the section is
  // explicitly named, else LblNone (because we don't know).
  struct cbl_label_t *p, label = { section? LblParagraph : LblNone, section };
  assert(strlen(name) < sizeof(label.name)); // caller ensures
  strcpy(label.name, name);
  if( label.type == LblNone ) assert(label.parent == 0);

  p = symbol_label_add(PROGRAM, &label);
  assert(p);

  const char *sect_name = section? cbl_label_of(symbol_at(section))->name : NULL;
  procedure_reference_add(sect_name, p->name, yylineno, current.program_section());

  return p;
}

void
current_t::repository_add_all() {
  assert( !programs.empty() );
  auto& repository = programs.top().function_repository;
  std::copy( function_descrs, function_descrs_end,
                  std::inserter(repository, repository.begin()) );
}

/*
 * A function is added to the symbol table when first named, in Identification
 * Division. It's also added to the current list of UDFs in current_t::udfs.
 * Its return type and parameters, if any, are defined later, in Procedure
 * Division.  When they are parsed, we call udf_update to finalize the
 * functions's descriptor, giving us enough information to validate the
 * arguments at point of invocation.
 */
void
current_t::udf_update( const ffi_args_t *ffi_args ) {
  auto L = cbl_label_of(symbol_at(program_index()));
  assert(L);
  assert(L->type == LblFunction);
  assert(L->returning);
  if( ! ffi_args ) return;
  assert(ffi_args->elems.size() < sizeof(function_descr_t::types));

  auto returning = cbl_field_of(symbol_at(L->returning));
  auto key = function_descr_t::init(L->name);
  auto func = udfs.find(key);
  assert(func != udfs.end());

  function_descr_t udf = *func;

  udf.ret_type = returning->type;
  udf.token = ffi_args->elems.empty()? FUNCTION_UDF_0 : FUNCTION_UDF;
  auto types = ffi_args->parameter_types();
  strcpy(udf.types, types);

  std::transform( ffi_args->elems.begin(), ffi_args->elems.end(),
		  std::back_inserter(udf.linkage_fields),
		  []( const cbl_ffi_arg_t& arg ) {
		    return function_descr_arg_t( field_index( arg.refer.field ),
						 arg.crv, arg.optional );
		  } );

  udfs.erase(func);
  auto result = udfs.insert(udf);
  assert(result.second);
}

bool
current_t::udf_args_valid( const cbl_label_t *L,
			   const std::list<cbl_refer_t>& args,
			   std::vector<function_descr_arg_t>& params /*out*/ )
{
  auto key = function_descr_t::init(L->name);
  auto func = udfs.find(key);
  assert(func != udfs.end());
  function_descr_t udf = *func;
  params = udf.linkage_fields;

  if( udf.linkage_fields.size() < args.size() ) {
    auto loc = symbol_field_location(field_index(args.back().field));
    error_msg(loc, "too many parameters for UDF %s", L->name);
    return false;
  }

  size_t i = 0;
  for( cbl_refer_t arg : args ) {
    if( arg.field ) { // else omitted
      auto tgt = cbl_field_of(symbol_at(udf.linkage_fields.at(i).isym));
      if( ! valid_move(tgt, arg.field) ) {
	auto loc = symbol_field_location(field_index(arg.field));
	error_msg(loc, "FUNCTION %s arg %zu, '%s' cannot be passed to %s, type %s",
		  L->name, i, arg.field->pretty_name(),
		  tgt->pretty_name(), 3 + cbl_field_type_str(tgt->type) );
	return false;
      }
    }
    i++;
  }
  return true;
}

bool
current_t::repository_add( const char name[]) {
  assert( !programs.empty() );
  function_descr_t arg = function_descr_t::init(name);
  auto parg = std::find( function_descrs, function_descrs_end, arg );
  if( parg == function_descrs_end ) return false;
  auto p = programs.top().function_repository.insert(*parg);
  if( yydebug ) {
    for( auto descr : programs.top().function_repository ) {
      dbgmsg("%s:%d: %-20s %-20s %-20s", __func__, __LINE__,
	    keyword_str(descr.token), descr.name, descr.cname);
    }
  }
  return p.second;
}

int
current_t::repository_in( const char name[]) {
  assert( !programs.empty() );
  auto isym = programs.top().program_index;
  // possible to call self
  auto self = cbl_label_of(symbol_at(isym));
  if( self->type == LblFunction ) {
    if( 0 == strcasecmp(self->name, name) ) {
      return FUNCTION_UDF;
    }
  }
  function_descr_t arg = function_descr_t::init(name);
  auto repository = programs.top().function_repository;
  auto p = repository.find(arg);
  return p != repository.end()? p->token : 0;
}

int repository_function_tok( const char name[] ) {
  return current.repository_in(name);
}

function_descr_t
function_descr_t::init( int isym ) {
  function_descr_t descr = { FUNCTION_UDF_0 };
  descr.ret_type = FldInvalid;
  auto L = cbl_label_of(symbol_at(isym));
  bool ok = namcpy(YYLTYPE(), descr.name, L->name);
  gcc_assert(ok);
  return descr;
}

arith_t::arith_t( cbl_arith_format_t format, refer_list_t * refers )
  : format(format), on_error(NULL), not_error(NULL)
{
  std::copy( refers->refers.begin(), refers->refers.end(), back_inserter(A) );
  refers->refers.clear();
  delete refers;
}


cbl_key_t::cbl_key_t( const sort_key_t& that )
  : ascending(that.ascending)
  , nfield(that.fields.size())
  , fields(NULL)
{
  if( nfield > 0 ) {
    fields = new cbl_field_t* [nfield];
    std::copy(that.fields.begin(), that.fields.end(), fields);
  }
}

static cbl_refer_t *
ast_op( cbl_refer_t *lhs, char op, cbl_refer_t *rhs ) {
  assert(lhs);
  assert(rhs);
  if( ! (is_numeric(lhs->field) && is_numeric(rhs->field)) ) {
    // If one of the fields isn't numeric, allow for index addition.
    switch(op) {
    case '+':
    case '-':
      // Simple addition OK for table indexes.
      if( lhs->field->type == FldIndex || rhs->field->type == FldIndex ) {
        goto ok;
      }
    }

    auto f  = !is_numeric(lhs->field)? lhs->field : rhs->field;
    auto loc = symbol_field_location(field_index(f));
    error_msg(loc, "'%s' is not numeric", f->name);
    return NULL;
  }
 ok:
  cbl_field_t skel = determine_intermediate_type( *lhs, op, *rhs );
  cbl_refer_t *tgt = new_reference_like(skel);
  if( !mode_syntax_only() ) {
    parser_op( *tgt, *lhs, op, *rhs, current.compute_label() );
  }
  return tgt;
}

static void
ast_add( arith_t *arith ) {
  size_t nC = arith->tgts.size(), nA = arith->A.size();
  std::vector <cbl_num_result_t> C(nC);
  cbl_num_result_t *pC;
  std::vector <cbl_refer_t> A(nA);
  cbl_refer_t *pA;

  pC = use_any(arith->tgts, C);
  pA = use_any(arith->A, A);

  parser_add( nC, pC, nA, pA, arith->format, arith->on_error, arith->not_error );

  ec_type_t handled = arith->on_error || arith->not_error ? ec_size_e : ec_none_e;
  current.declaratives_evaluate(handled);
}

static bool
ast_subtract( arith_t *arith ) {
  size_t nC = arith->tgts.size(), nA = arith->A.size(), nB = arith->B.size();
  std::vector <cbl_refer_t> A(nA);
  std::vector <cbl_refer_t> B(nB);
  std::vector <cbl_num_result_t> C(nC);

  cbl_refer_t *pA = use_any(arith->A, A);
  cbl_refer_t *pB = use_any(arith->B, B);
  cbl_num_result_t *pC = use_any(arith->tgts, C);

  parser_subtract( nC, pC, nA, pA, nB, pB, arith->format, arith->on_error, arith->not_error );

  ec_type_t handled = arith->on_error || arith->not_error ? ec_size_e : ec_none_e;
  current.declaratives_evaluate(handled);
  return true;
}

static bool
ast_multiply( arith_t *arith ) {
  size_t nC = arith->tgts.size(), nA = arith->A.size(), nB = arith->B.size();
  std::vector <cbl_refer_t> A(nA);
  std::vector <cbl_refer_t> B(nB);
  std::vector <cbl_num_result_t> C(nC);

  cbl_refer_t *pA = use_any(arith->A, A);
  cbl_refer_t *pB = use_any(arith->B, B);
  cbl_num_result_t *pC = use_any(arith->tgts, C);

  parser_multiply( nC, pC, nA, pA, nB, pB, arith->on_error, arith->not_error );

  ec_type_t handled = arith->on_error || arith->not_error ? ec_size_e : ec_none_e;
  current.declaratives_evaluate(handled);
  return true;
}

static bool
ast_divide( arith_t *arith ) {
  size_t nC = arith->tgts.size(), nA = arith->A.size(), nB = arith->B.size();
  std::vector <cbl_refer_t> A(nA);
  std::vector <cbl_refer_t> B(nB);
  std::vector <cbl_num_result_t> C(nC);

  cbl_refer_t *pA = use_any(arith->A, A);
  cbl_refer_t *pB = use_any(arith->B, B);
  cbl_num_result_t *pC = use_any(arith->tgts, C);

  parser_divide( nC, pC, nA, pA, nB, pB,
                 arith->remainder, arith->on_error, arith->not_error );

  ec_type_t handled = arith->on_error || arith->not_error ? ec_size_e : ec_none_e;
  current.declaratives_evaluate(handled);
  return true;
}

/*
 * Populate a parser API struct from lists built up by the parser.
 * The API doesn't use STL containers or classes that exist only for
 * the convenience of the parser.
*/
struct stringify_src_t : public cbl_string_src_t {
 stringify_src_t( const refer_marked_list_t& marked = refer_marked_list_t() )
   : cbl_string_src_t( marked.marker? *marked.marker : null_reference,
                       marked.refers.size(),
                       new cbl_refer_t[marked.refers.size()] )
  {
    std::copy( marked.refers.begin(), marked.refers.end(), inputs );
  }

  static void dump( const cbl_string_src_t& src ) {
    dbgmsg( "%s:%d:, %zu inputs delimited by %s:", __func__, __LINE__,
           src.ninput,
           src.delimited_by.field? field_str(src.delimited_by.field) : "SIZE" );
    std::for_each(src.inputs, src.inputs + src.ninput, dump_input);
  }

 protected:
  static void dump_input( const cbl_refer_t& refer ) {
    yywarn( "%s:\t%s", __func__, field_str(refer.field) );
  }
};

void
stringify( refer_collection_t *inputs,
           cbl_refer_t into, cbl_refer_t pointer,
           cbl_label_t  *on_error,
           cbl_label_t *not_error )
{
  std::vector <stringify_src_t> sources(inputs->lists.size());

  if( inputs->lists.back().marker == NULL ) {
    inputs->lists.back().marker = cbl_refer_t::empty();
  }
  assert( inputs->lists.back().marker );
  std::copy( inputs->lists.begin(), inputs->lists.end(), sources.begin() );
  parser_string( into, pointer, sources.size(), sources.data(), on_error, not_error );
}

void
unstringify( cbl_refer_t& src,
             refer_list_t *delimited,
             unstring_into_t * into,
             cbl_label_t  *on_error,
             cbl_label_t *not_error )
{
  size_t ndelimited = delimited? delimited->size() : 0;
  cbl_refer_t *pdelimited = NULL;
  std::vector <cbl_refer_t> delimiteds(ndelimited);
  if( ndelimited > 0 ) {
    pdelimited = use_any( delimited->refers, delimiteds );
  }

  std::vector <cbl_refer_t> outputs(into->size());
  into->use_list( outputs, unstring_tgt_t::tgt_of );

  std::vector <cbl_refer_t> delimiters(into->size());
  into->use_list( delimiters, unstring_tgt_t::delimiter_of );

  std::vector <cbl_refer_t> counts(into->size());
  into->use_list( counts, unstring_tgt_t::count_of );

  parser_unstring( src,
                   ndelimited, pdelimited,
                   // into
                   outputs.size(),
                   outputs.data(), delimiters.data(), counts.data(),
                   into->pointer, into->tally,
                   on_error, not_error );
  delete into;
}

static const char *
data_section_str( data_section_t section ) {
  switch(section) {
  case not_data_datasect_e:
    return "NONE";
  case local_storage_datasect_e:
    return "LOCAL";
  case file_datasect_e:
    return "FILE";
  case working_storage_datasect_e:
    return "WORKING";
  case linkage_datasect_e:
    return "LINKAGE";
  }
  gcc_unreachable();
  return NULL;
}

static bool
current_data_section_set(const YYLTYPE& loc,  data_section_t data_section ) {
  // order is mandatory
  if( data_section < current_data_section ) {
    error_msg(loc, "%s SECTION must precede %s SECTION",
	      data_section_str(data_section),
	      data_section_str(current_data_section));
    return false;
  }

  cbl_section_type_t type = file_sect_e;

  switch(data_section) {
  case not_data_datasect_e:
    gcc_unreachable();
    break;
  case file_datasect_e:
    type = file_sect_e;
    break;
  case working_storage_datasect_e:
    type = working_sect_e;
    break;
  case local_storage_datasect_e:
    type = local_sect_e;
    break;
  case linkage_datasect_e:
    type = linkage_sect_e;
    break;
  }

  cbl_section_t section = { type, yylineno, NULL };

  if( ! symbol_section_add(PROGRAM, &section) ) {
    error_msg(loc, "could not add section %s to program %s, exists line %d",
              section.name(), current.program()->name,
              symbol_section(PROGRAM, &section)->line );
    return false;
  }

  current_data_section = data_section ;
  return true;
}

void apply_declaratives() {
  // look for declaratives for this procedure, and all procedures
  bool tf[2] = { false, true };
  for( bool *yn = tf; yn < tf + COUNT_OF(tf); yn++ ) {
    auto declaratives = current.debugging_declaratives(*yn);
    for( auto p  = declaratives.begin() ;
         p != declaratives.end(); p++ ) {
      // TODO: delarative for PARA OF SECTION
      cbl_label_t *label = symbol_label(PROGRAM, LblNone, 0, p->c_str());
      assert(label);
      parser_perform(label);
    }
  }
}
#define FIG_CONST(X) constant_of(constant_index((X)))

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"

int warn_abi_version = -1;
int cp_unevaluated_operand;
void
lang_check_failed (const char* file, int line, const char* function) {}

#pragma GCC diagnostic pop

void ast_inspect( cbl_refer_t& input, bool backward, ast_inspect_list_t& inspects ) {
  if( yydebug ) {
    dbgmsg("%s:%d: INSPECT %zu operations on %s, line %d", __func__, __LINE__,
          inspects.size(), input.field->name, yylineno);
  }
  std::for_each(inspects.begin(), inspects.end(), dump_inspect);
  auto array = inspects.as_array();
  parser_inspect( input, backward, inspects.size(), array );
  delete[] array;
}

static const char *
cbl_refer_str( char output[], const cbl_refer_t& R ) {
  sprintf( output, "refer = %s %s %s",
           R.field? field_str(R.field) : "(none)",
           R.is_table_reference()? "(table)" : "",
           R.is_refmod_reference()? "(refmod)" : "" );
  return output;
}

static void
dump_inspect_match( const cbl_inspect_match_t& M ) {
  static char fields[3][4 * 64];
  cbl_refer_str(fields[0], M.matching);
  cbl_refer_str(fields[1], M.before.identifier_4);
  cbl_refer_str(fields[2], M.after.identifier_4);

  yywarn( "matching %s \n\t\tbefore %s%s \n\t\tafter  %s%s",
         fields[0],
         M.before.initial? "initial " : "", fields[1],
         M.after.initial?  "initial " : "", fields[2] );
}

static void
dump_inspect_replace( const cbl_inspect_replace_t& R ) {
  static char fields[4][4 * 64];
  cbl_refer_str(fields[0], R.matching);
  cbl_refer_str(fields[1], R.before.identifier_4);
  cbl_refer_str(fields[2], R.after.identifier_4);
  cbl_refer_str(fields[3], R.replacement);

  yywarn( "matching    %s \n\treplacement %s\n\t\tbefore %s%s \n\t\tafter  %s%s",
         fields[0], fields[3],
         R.before.initial? "initial " : "", fields[1],
         R.after.initial?  "initial " : "", fields[2] );
}

static const char *
bound_str( cbl_inspect_bound_t bound ) {
  switch(bound) {
  case bound_characters_e: return "characters";
  case bound_all_e: return "all";
  case bound_first_e: return "first";
  case bound_leading_e: return "leading";
  case bound_trailing_e: return "trailing";
  }
  return "bound?";
}

/*
 * INITIALIZE
 */
static data_category_t
data_category_of( const cbl_refer_t& refer ) {
  assert(refer.field);
  switch( refer.field->type ) {
  case FldInvalid:
    assert(refer.field->type != FldInvalid);
    return data_category_none;

  case FldGroup:
    return data_category_none;

  case FldLiteralA:
  case FldAlphanumeric:
    return refer.field->has_attr(all_alpha_e)?
      data_alphabetic_e : data_alphanumeric_e;

  case FldNumericBinary:
  case FldFloat:
  case FldNumericBin5:
  case FldPacked:
  case FldNumericDisplay:
  case FldLiteralN:
    return data_numeric_e;

  case FldNumericEdited:
    return data_numeric_edited_e;
  case FldAlphaEdited:
    return data_alphanumeric_edited_e;

  case FldPointer:
    return data_data_pointer_e;

  case FldClass:
  case FldConditional:
  case FldForward:
  case FldIndex:
  case FldSwitch:
  case FldDisplay:
  case FldBlob:
    return data_category_none;
  }
  gcc_unreachable();
  return data_category_none;
}

static bool
valid_target( const cbl_refer_t& refer ) {
  assert(refer.field);
  switch( refer.field->type ) {
  case FldInvalid:
    assert(refer.field->type != FldInvalid);
    return false;
  case FldGroup:
  case FldAlphanumeric:
  case FldNumericBinary:
  case FldFloat:
  case FldNumericBin5:
  case FldPacked:
  case FldNumericDisplay:
  case FldNumericEdited:
  case FldAlphaEdited:
  case FldPointer:
    return true;
  case FldLiteralA:
  case FldLiteralN:
  case FldClass:
  case FldConditional:
  case FldForward:
  case FldIndex:
  case FldSwitch:
  case FldDisplay:
  case FldBlob:
    return false;
  }
  gcc_unreachable();
  return false;
}

static REAL_VALUE_TYPE
numstr2i( const char input[], radix_t radix ) {
  REAL_VALUE_TYPE output;
  size_t integer = 0;
  int erc=0;

  switch( radix ) {
  case decimal_e: { // Use decimal point for comma, just in case.
      auto local = xstrdup(input);
      if( !local ) { erc = -1; break; }
      std::replace(local, local + strlen(local), ',', '.');
      real_from_string3 (&output, local, TYPE_MODE (float128_type_node));
    }
    break;
  case hexadecimal_e:
    erc = sscanf(input, "%zx", &integer);
    real_from_integer (&output, VOIDmode, integer, UNSIGNED);
    break;
  case boolean_e:
    for( const char *p = input; *p != '\0'; p++ ) {
      if( ssize_t(8 * sizeof(integer) - 1) < p - input ) {
        yywarn("'%s' was accepted as %d", input, integer);
        break;
      }
      switch(*p) {
        case '0':
        case '1':
          integer = (integer << (p - input));
          integer |= ((*p) == '0' ? 0 : 1);
          break;
      default:
        yywarn("'%s' was accepted as %d", input, integer);
	break;
      }
    }
    real_from_integer (&output, VOIDmode, integer, UNSIGNED);
    return output;
  }
  if( erc == -1 ) {
    yywarn("'%s' was accepted as %lld", input, output);
  }
  return output;
}

static inline cbl_field_t *
new_literal( const char initial[], enum radix_t radix ) {
  auto attr = constant_e;

  switch( radix ) {
  case decimal_e:
    break;
  case hexadecimal_e:
    attr = hex_encoded_e;
    break;
  case boolean_e:
    attr = bool_encoded_e;
    break;
  }
  return new_literal(strlen(initial), initial,
                     cbl_field_attr_t(constant_e | attr));
}

class is_elementary_type { // for INITIALIZE purposes
  bool with_filler;
public:
  is_elementary_type( bool with_filler ) : with_filler(with_filler) {}

  bool operator()( const symbol_elem_t& elem ) const {
    if( elem.type != SymField ) return false;
    const cbl_field_t *f = cbl_field_of(&elem);
    if( symbol_redefines(f) ) return false;
    return ( f->has_attr(filler_e) && with_filler )
      || ::is_elementary(f->type);
  }
};

size_t end_of_group( size_t igroup );

static std::list<cbl_refer_t>
symbol_group_data_members( cbl_refer_t refer, bool with_filler ) {
  std::list<cbl_refer_t> refers;
  refers.push_front( refer );

  if( refer.field->type != FldGroup ) return refers;

  class refer_of : public cbl_refer_t {
   public:
    refer_of( const cbl_refer_t& refer ) : cbl_refer_t(refer) {}
    cbl_refer_t operator()( symbol_elem_t& elem ) {
      this->field = cbl_field_of(&elem); // preserve subscript/refmod
      return *this;
    }
  };

  size_t igroup = field_index(refer.field), eogroup = end_of_group(igroup);
  std::list<symbol_elem_t> elems;
  is_elementary_type is_elem(with_filler);

  std::copy_if( symbols_begin(igroup), symbols_begin(eogroup),
                std::back_inserter(elems), [is_elem]( const symbol_elem_t& elem ) {
                  return is_elem(elem) || cbl_field_of(&elem)->occurs.ntimes() > 0; } );
  std::transform( elems.begin(), elems.end(),
                  std::back_inserter(refers), refer_of(refer) );
  return refers;
}

struct expand_group : public std::list<cbl_refer_t> {
  static cbl_refer_t referize( cbl_field_t *field ) {
    return cbl_refer_t(field);
  }
  bool with_filler;
  expand_group( bool with_filler ) : with_filler(with_filler) {}

  void operator()( const cbl_refer_t& refer ) {
    assert(refer.field);
    if( refer.field->type != FldGroup ) {
      push_back(refer);
      return;
    }
    std::list<cbl_refer_t> members = symbol_group_data_members( refer,
                                                                with_filler );
    std::copy( members.begin(), members.end(), back_inserter(*this) );
  }
};


static const char * initial_default_value;
       const char * wsclear() { return initial_default_value; }

void
wsclear( char ch ) {
  static char byte = ch;
  initial_default_value = &byte;
  current.program_needs_initial();
}

static void
initialize_allocated( cbl_refer_t input ) {
  cbl_num_result_t result = { truncation_e, input };
  std::list<cbl_num_result_t> results;
  results.push_back(result);
  initialize_statement(results, true,
		       data_category_all, category_map_t());
}

static int
initialize_with( cbl_refer_t tgt ) {
  if( tgt.field->type == FldPointer ) return ZERO;
  if( tgt.is_refmod_reference() ) return SPACES;
  return is_numeric(tgt.field)? ZERO : SPACES;
}

static bool
initialize_one( cbl_num_result_t target, bool with_filler,
                data_category_t value_category,
                const category_map_t& replacements,
                bool explicitly )
{
  cbl_refer_t& tgt( target.refer );
  if( ! valid_target(tgt) ) return false;

  // Rule 1 c: is valid for VALUE, REPLACING, or DEFAULT
  // If no VALUE (category none), set to blank/zero.
  if( value_category == data_category_none && replacements.empty() ) {
    auto token = initialize_with(tgt);
    auto src = constant_of(constant_index(token));
    cbl_refer_t source(src);
    auto s = wsclear();
    if( s ) {
      char ach[5];
      int v = *s;
      sprintf(ach, "%d", v);
      source.field = new_literal(ach);
      source.addr_of = true;
    }

    if( tgt.field->type == FldPointer ) {
      parser_set_pointers(1, &tgt, source);
    } else {
      parser_move(tgt, src, current_rounded_mode());
    }
    return true;
  }

  /*
   *  Either VALUE or REPLACING specified.
   */

  if( value_category == data_category_all ||
      value_category == data_category_of(tgt) ) {
    // apply any applicable VALUE
    if( explicitly || tgt.field->data.initial ) {
      assert( with_filler || !tgt.field->has_attr(filler_e) );
      if( tgt.field->data.initial ) {
        parser_initialize(tgt);
      }
    }
  }

  // apply REPLACING, possibly overwriting VALUE
  // N.B., may be wrong:
  /*
   * "If the data item does not qualify as a receiving-operand because of the
   *  VALUE phrase, but does qualify because of the REPLACING phrase ..."
  */
  auto r = replacements.find(data_category_of(tgt));
  if( r != replacements.end() ) {
    parser_move( tgt, *r->second );

    return true;
  }

  return true;
}

typedef std::pair<cbl_field_t*,cbl_field_t*> field_span_t;
typedef std::pair<size_t, size_t>  cbl_bytespan_t;

/*
 * After the 1st record is initialized, copy it to the others.
 */
static bool
initialize_table( cbl_num_result_t target,
		  size_t nspan, const cbl_bytespan_t spans[],
		  const std::list<cbl_subtable_t>& subtables )
{
  assert( target.refer.nsubscript == dimensions(target.refer.field) );
  const cbl_refer_t& src( target.refer );
  size_t n( src.field->occurs.ntimes());
  assert( 0 < n );

  size_t isym( field_index(src.field) );
  std::vector <cbl_subtable_t> tbls(subtables.size());
  std::copy( subtables.begin(), subtables.end(), tbls.begin() );
  parser_initialize_table( n, src, nspan, spans, isym, tbls.size(), tbls.data() );
  return true;
}

static cbl_refer_t
synthesize_table_refer( cbl_refer_t tgt ) {
  // For a table, use supplied subscripts or start with 1.
  auto ndim( dimensions(tgt.field) );
  if( tgt.nsubscript < ndim ) { // it's an incomplete table
    std::vector <cbl_refer_t> subscripts(ndim);
    for( size_t i=0; i < ndim; i++ ) {
      if( i < tgt.nsubscript ) {
	subscripts[i] = tgt.subscripts[i];
	continue;
      }
      subscripts[i].field = new_tempnumeric();
      parser_set_numeric(subscripts[i].field, 1);
    }
    return cbl_refer_t( tgt.field, subscripts.size(), subscripts.data() );
  }
  return tgt;
}

static size_t
group_offset( const cbl_field_t *field ) {
  if( field->parent ) {
    auto e = symbol_at(field->parent);
    if( e->type == SymField ) {
      auto parent = cbl_field_of(e);
      return field->offset - parent->offset;
    }
  }
  return field->offset;
}

static bool
initialize_statement( const cbl_num_result_t& target, bool with_filler,
                      data_category_t value_category,
                      const category_map_t& replacements,
                      size_t depth = 0 )
{
  const cbl_refer_t& tgt( target.refer );
  assert(dimensions(tgt.field) == tgt.nsubscript || 0 < depth);
  assert(!is_literal(tgt.field));

  if( tgt.field->type == FldGroup ) {
    if( tgt.field->data.initial )   goto initialize_this;
    if( tgt.is_refmod_reference() ) goto initialize_this;
    // iterate over group memebers
    auto imember = field_index(tgt.field);
    auto eogroup = end_of_group(imember);
    bool fOK = true;
    std::list<cbl_field_t*> members;
    std::list<cbl_subtable_t> subtables;

    while( ++imember < eogroup ) {
        auto e = symbol_at(imember);
        if( e->type != SymField ) continue;
        auto f = cbl_field_of(e);
        if( ! (f->type == FldGroup || is_elementary(f->type)) ) continue;
        if( ! symbol_redefines(f) ) {
	  members.push_back(f);
	  if( is_table(f) ) {
	    size_t offset = group_offset(f);
	    subtables.push_back( cbl_subtable_t { offset, imember } );
	  }
          cbl_num_result_t next_target(target);
	  next_target.refer.field = f;
          // recurse on each member, which might be a table or group
          fOK = fOK && initialize_statement( next_target, with_filler, value_category,
                                             replacements, 1 + depth );
        }
        if( f->type == FldGroup ) {
          imember = end_of_group(imember) - 1;
        }
    }

    if( fOK && is_table(tgt.field) ) {
      cbl_num_result_t output = { target.rounded, synthesize_table_refer(tgt) };
      if( tgt.nsubscript < output.refer.nsubscript ) { // tgt is whole table
	std::list<field_span_t> field_spans;
	static const field_span_t empty_span = { NULL, NULL };
	field_span_t span = empty_span;
	bool honor_filler = false;
	// construct non-filler field spans
	for( auto member : members ) {
	  if( !with_filler && member->has_attr(filler_e) ) {
	    if( span.first ) { // conclude the span and begin to skip filler
	      field_spans.push_back(span);
	      span = empty_span;
	      honor_filler = true;
	    }
	    continue;
	  }
	  if( span.first ) {
	    span.second = member; // extend the span
	  } else {
	    span.first = member;  // start a new span
	  }
	}
	if( span.first ) {
	  field_spans.push_back(span);
	}
	// convert field spans to byte ranges
	std::vector <cbl_bytespan_t> ranges( field_spans.size() );
	size_t nrange = 0;
	if( honor_filler ) {
	  nrange = ranges.size();
	  std::transform( field_spans.begin(), field_spans.end(), ranges.begin(),
			  []( const auto& span ) {
			    size_t first, second;
			    first = second = group_offset(span.first);
			    if( ! span.second ) {
			      second += std::max(span.first->data.capacity,
						 span.first->data.memsize);
			    } else {
			      second = group_offset(span.second)
				     - group_offset(span.first);
			      second += std::max(span.second->data.capacity,
						 span.second->data.memsize);
			    }
			    return std::make_pair(first, second);
			  } );
	}
	return initialize_table( output, nrange, ranges.data(), subtables );
      }
    }
    return fOK;
  }

  if( !is_elementary(tgt.field->type) ) return false;

  assert(is_elementary(tgt.field->type));
  assert(data_category_of(tgt) != data_category_none);

  /*
   * Initialize elementary field.
   */

 initialize_this:
  // Cannot initialize constants
  if( is_constant(tgt.field) ) {
    auto loc = symbol_field_location(field_index(tgt.field));
    error_msg(loc, "%s is constant", name_of(tgt.field));
    return false;
  }
  // Ignore filler unless instructed otherwise.
  if( !with_filler && tgt.field->has_attr(filler_e) ) return true;

  cbl_num_result_t output = { target.rounded, synthesize_table_refer(tgt) };

  bool fOK = initialize_one( output, with_filler, value_category,
			     replacements, depth == 0 );

  if( fOK && is_table(tgt.field) ) {
    return initialize_table( output,
			     0, NULL, std::list<cbl_subtable_t>() );
  }

  return fOK;
}

const char *
data_category_str( data_category_t category ) {
  switch(category) {
  case data_category_none: return "category_none";
  case data_category_all: return "category_all";
  case data_alphabetic_e: return "alphabetic";
  case data_alphanumeric_e: return "alphanumeric";
  case data_alphanumeric_edited_e: return "alphanumeric_edited";
  case data_boolean_e: return "data_boolean";
  case data_data_pointer_e: return "data_data_pointer";
  case data_function_pointer_e: return "data_function_pointer";
  case data_msg_tag_e: return "data_msg_tag";
  case data_dbcs_e: return "dbcs";
  case data_egcs_e: return "egcs";
  case data_national_e: return "national";
  case data_national_edited_e: return "national_edited";
  case data_numeric_e: return "numeric";
  case data_numeric_edited_e: return "numeric_edited";
  case data_object_referenc_e: return "data_object_referenc";
  case data_program_pointer_e: return "data_program_pointer";
 }
  return "???";
}

static void
initialize_statement( std::list<cbl_num_result_t>& tgts, bool with_filler,
                     data_category_t value_category,
                     const category_map_t& replacements) {

  bool is_refmod = std::any_of( tgts.begin(), tgts.end(),
				[]( const auto& tgt ) {
				  return tgt.refer.is_refmod_reference();
				} );
  if( false && is_refmod ) { // refmod seems valid per ISO
    dbgmsg("INITIALIZE cannot initialize a refmod");
    return;
  }

  for( auto tgt : tgts ) {
    initialize_statement( tgt, with_filler, value_category,
                         replacements );
  }
  tgts.clear();
}

static void
dump_inspect_oper( const cbl_inspect_oper_t& op ) {
  dbgmsg("\t%s: %zu \"matches\", %zu \"replaces\"",
        bound_str(op.bound),
        op.matches? op.n_identifier_3 : 0, op.replaces? op.n_identifier_3 : 0);
  if( op.matches )
    std::for_each(op.matches, op.matches + op.n_identifier_3, dump_inspect_match);
  if( op.replaces )
    std::for_each(op.replaces, op.replaces + op.n_identifier_3, dump_inspect_replace);
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"

static void
dump_inspect( const cbl_inspect_t& I ) {
  if( !yydebug ) return;
  if( I.tally.field ) {
    fprintf( stderr, "\tTALLYING to %s %s %s:\n",
             field_str(I.tally.field),
             I.tally.is_table_reference()? "(table)" : "",
             I.tally.is_refmod_reference()? "(refmod)" : "" );
  } else {
    fprintf( stderr, "\tREPLACING:\n" );
  }
  std::for_each( I.opers, I.opers + I.nbound, dump_inspect_oper );
}
#pragma GCC diagnostic pop

#include <iterator>

struct declarative_file_list_t : protected cbl_declarative_t {
  declarative_file_list_t( const cbl_declarative_t& d )
    : cbl_declarative_t(d)
    {
      if( nfile > 0 )
        assert(d.files[0] == this->files[0]);
    }
  static std::ostream&
  splat( std::ostream& os, const declarative_file_list_t& dcl ) {
    static int i=0;

    os << "static size_t dcl_file_list_" << i++
       << "[" << dcl.nfile << "] = { ";
    std::ostream_iterator<size_t> out(os, ", ");
    std::copy( dcl.files, dcl.files + dcl.nfile, out );
    return os << "};";
  }
};

std::ostream&
operator<<( std::ostream& os, const declarative_file_list_t& dcl ) {
  return dcl.splat( os, dcl );
}

static declarative_file_list_t
file_list_of( const cbl_declarative_t& dcl ) {
  return dcl;
}

std::ostream&
operator<<( std::ostream& os, const cbl_declarative_t& dcl ) {
  static int i=0;

  return os <<
    "\t{ " << dcl.section << ", "
           << std::boolalpha << dcl.global << ", "
           << ec_type_str(dcl.type) << ", "
           << dcl.nfile << ", "
           << "dcl_file_list_" << i++ << ", "
           << cbl_file_mode_str(dcl.mode) << " }"
           << std::flush;
}

void parser_add_declaratives( size_t n, cbl_declarative_t *declaratives) {
  const char *prog = cbl_label_of(symbol_at(PROGRAM))->name;
  char *filename = xasprintf("declaratives.%s.h", prog);
  std::ofstream os(filename);
  {
    std::ostream_iterator<declarative_file_list_t> out(os, "\n");
    std::transform( declaratives, declaratives + n, out, file_list_of );
  }
  os << "\nstatic cbl_declarative_base_t declaratives[] = {\n";
  std::ostream_iterator<cbl_declarative_t> out(os, ", \n");
  std::copy( declaratives, declaratives + n, out );
  os << "};\n" << std::endl;
}

cbl_field_t *
new_literal( const literal_t& lit, enum cbl_field_attr_t attr ) {
  bool zstring = lit.prefix[0] == 'Z';
  if( !zstring && lit.data[lit.len] != '\0' ) {
    dbgmsg("%s:%d: line %d, no NUL terminator '%-*.*s'{%zu/%zu}",
          __func__, __LINE__, yylineno,
          int(lit.len), int(lit.len),
          lit.data, strlen(lit.data), lit.len);
  }
  assert(zstring || lit.data[lit.len] == '\0');

  size_t attrs(attr);
  attrs |= constant_e;
  attrs |= literal_attr(lit.prefix);

  return new_literal(lit.len, lit.data, cbl_field_attr_t(attrs));
}

bool
cbl_file_t::validate_forward( size_t isym ) const {
  if( isym > 0 && FldForward == symbol_field_forward(isym)->type ) {
    auto loc = symbol_field_location(isym);
    error_msg(loc, "line %d: %s of %s is not defined",
              this->line, cbl_field_of(symbol_at(isym))->name,
              this->name );
    return false;
  }
  return true;
}

bool
cbl_file_t::validate_key( const cbl_file_key_t& key ) const {
  for( auto f = key.fields; f < key.fields + key.nfield; f++ ) {
    if( ! validate_forward(*f) ) return false;
  }
  return true;
}

bool
cbl_file_t::validate() const {
  size_t members[] = { user_status, vsam_status, record_length };
  bool tf = true;

  for( auto isym : members ) {
    if( ! validate_forward(isym) ) tf = false;
  }

  for( auto p = keys; p < keys + nkey; p++ ) {
    if( ! validate_key(*p) ) tf = false;
  }

  return tf;
}

bool
cbl_file_lock_t::mode_set( int token ) {
  switch( token ) {
  case MANUAL:    mode = manual_e; break;
  case RECORD:    mode = record_e; break;
  case AUTOMATIC: mode = automatic_e; break;
  default:
    return  false;
  }
  return true;
}

enum cbl_figconst_t
cbl_figconst_of( const char *value ) {
  struct values_t {
    const char *value; cbl_figconst_t type;
  } static const values[] = {
    { constant_of(constant_index(ZERO))->data.initial, zero_value_e },
    { constant_of(constant_index(SPACES))->data.initial, space_value_e },
    { constant_of(constant_index(HIGH_VALUES))->data.initial, high_value_e },
    { constant_of(constant_index(LOW_VALUES))->data.initial, low_value_e },
    { constant_of(constant_index(QUOTES))->data.initial, quote_value_e },
    { constant_of(constant_index(NULLS))->data.initial, null_value_e },
  }, *eovalues = values + COUNT_OF(values);

  auto p = std::find_if( values, eovalues,
                         [value]( const values_t& elem ) {
                           return elem.value == value;
                         } );

  return p == eovalues? normal_value_e : p->type;
}

cbl_field_attr_t
literal_attr( const char prefix[] ) {
  switch(strlen(prefix)) {
  case 0: return none_e;

  case 1:
    switch(prefix[0]) {
    case 'B': return bool_encoded_e;
    case 'N': cbl_unimplemented("National"); return none_e;
    case 'X': return hex_encoded_e;
    case 'Z': return quoted_e;
    }
    break;

  case 2:
    switch(prefix[1]) {
    case 'X':
      switch(prefix[0]) {
      case 'B': return cbl_field_attr_t(hex_encoded_e | bool_encoded_e);
      case 'N': cbl_unimplemented("National"); return none_e;
      }
      break;
    }
  }

  // must be [BN]X
  cbl_internal_error("'%s': invalid literal prefix", prefix);
  gcc_unreachable();
  return none_e;
}

bool
cbl_field_t::has_subordinate( const cbl_field_t *that ) const {
  while( (that = parent_of(that)) != NULL ) {
    if( field_index(this) == field_index(that) ) return true;
  }
  return false;
}

const char *
cbl_field_t::value_str() const {
    if( data.etc_type == cbl_field_data_t::value_e )
	return string_of( data.value_of() );
    return "???";
}

static const cbl_division_t not_syntax_only = cbl_division_t(-1);
             cbl_division_t cbl_syntax_only = not_syntax_only;

void
mode_syntax_only( cbl_division_t division ) {
  cbl_syntax_only = division;
}

// Parser moves to syntax-only mode if data-division errors preclude compilation.
bool
mode_syntax_only() {
  return cbl_syntax_only != not_syntax_only
    && cbl_syntax_only <= current_division;
}

void
cobol_dialect_set( cbl_dialect_t dialect ) {
  cbl_dialect = dialect;
  if( dialect & dialect_ibm_e ) cobol_gcobol_feature_set(feature_embiggen_e);
}
cbl_dialect_t cobol_dialect() { return cbl_dialect; }

static bool internal_ebcdic_locked = false;

void internal_ebcdic_lock() {
  internal_ebcdic_locked = true;
}
void internal_ebcdic_unlock() {
  internal_ebcdic_locked = false;
}

bool
cobol_gcobol_feature_set( cbl_gcobol_feature_t gcobol_feature, bool on ) {
  if( gcobol_feature == feature_internal_ebcdic_e ) {
    if( internal_ebcdic_locked ) return false;
  }
  if( on ) {
    cbl_gcobol_features |= gcobol_feature;
  } else {
    cbl_gcobol_features &= ~gcobol_feature;
  }
  return true;
}

static bool
literal_refmod_valid( YYLTYPE loc, const cbl_refer_t& r ) {
  if( r.field->has_attr(any_length_e) ) return true;

  const cbl_span_t& refmod(r.refmod);

  if( ! is_literal(refmod.from->field) ) {
    if( ! refmod.len ) return true;
    if( ! is_literal(refmod.len->field) ) return true;
    auto edge = refmod.len->field->as_integer();
    if( 0 < edge ) {
      if( edge-1 < r.field->data.capacity ) return true;
    }
    // len < 0 or not: 0 < from + len <= capacity
    error_msg(loc, "%s(%s:%zu) out of bounds, "
	           "size is %u",
	      r.field->name,
	      refmod.from->name(),
	      size_t(edge),
	      static_cast<unsigned int>(r.field->data.capacity) );
    return false;
  }

  auto edge = refmod.from->field->as_integer();
  if( edge > 0 ) {
    if( --edge < r.field->data.capacity ) {
      if( ! refmod.len ) return true;
      if( ! is_literal(refmod.len->field) ) return true;
      auto len = refmod.len->field->as_integer();
      if( len > 0 ) {
	edge += len;
	if( --edge < r.field->data.capacity ) return true;
      }
      // len < 0 or not: 0 < from + len <= capacity
      auto loc = symbol_field_location(field_index(r.field));
      error_msg(loc, "%s(%zu:%zu) out of bounds, "
		"size is %u",
		r.field->name,
		size_t(refmod.from->field->as_integer()),
		size_t(len),
		static_cast<unsigned int>(r.field->data.capacity) );
      return false;
    }
  }
  // not: 0 < from <= capacity
  error_msg(loc,"%s(%zu) out of bounds, size is %u",
	    r.field->name,
	    size_t(refmod.from->field->as_integer()),
	    static_cast<unsigned int>(r.field->data.capacity) );
  return false;
}

const cbl_field_t *
literal_subscript_oob( const cbl_refer_t& r, size_t& isub );

static bool
literal_subscripts_valid( YYLTYPE loc, const cbl_refer_t& name ) {
  static char subs[ 7 * 32 ], *esub = subs + sizeof(subs);
  char *p = subs;
  size_t isub;

  // Find subscript in the supplied refer
  const cbl_field_t *oob = literal_subscript_oob(name, isub);
  if( oob ) {
    const char *sep = "";
    for( auto r = name.subscripts; r < name.subscripts + name.nsubscript; r++ ) {
      snprintf( p, esub - p, "%s%s", sep, nice_name_of(r->field) );
      sep = " ";
    }

    const char *upper_phrase = "";
    if( ! oob->occurs.bounds.fixed_size() ) {
      static char ub[32] = "boo";
      sprintf(ub, " to %lu", oob->occurs.bounds.upper);
      upper_phrase = ub;
    }

    // X(0): subscript 1 of for out of range for 02 X OCCURS 4 to 6
    error_msg(loc, "%s(%s): subscript %zu out of range "
                   "for %s %s OCCURS %lu%s",
	      oob->name, subs, 1 + isub,
	      oob->level_str(), oob->name,
	      oob->occurs.bounds.lower, upper_phrase );
    return false;
  }
  return true;
}

static void
subscript_dimension_error( YYLTYPE loc, size_t nsub, const cbl_refer_t *scalar ) {
  if( 0 == dimensions(scalar->field) ) {
    error_msg(loc, "%zu subscripts provided for %s, "
              "which has no dimensions",
              nsub, scalar->name() );
  } else {
    error_msg(loc, "%zu subscripts provided for %s, "
              "which requires %zu dimensions",
              nsub, scalar->name(), dimensions(scalar->field) );
  }
}

static void
reject_refmod( YYLTYPE loc, cbl_refer_t scalar ) {
  if( scalar.is_refmod_reference() ) {
    error_msg(loc, "%s cannot be reference-modified here", scalar.name());
  }
}

static bool
require_pointer( YYLTYPE loc, cbl_refer_t scalar ) {
  if( scalar.field->type != FldPointer ) {
    error_msg(loc, "%s must have USAGE POINTER", scalar.name());
    return false;
  }
  return true;
}

static bool
require_numeric( YYLTYPE loc, cbl_refer_t scalar ) {
  if( ! is_numeric(scalar.field) ) {
    error_msg(loc, "%s must have numeric USAGE", scalar.name());
    return false;
  }
  return true;
}

/* eval methods */

eval_subject_t::eval_subject_t()
  : result( new_temporary(FldConditional) )
{
  labels.when = label("when");
  labels.yeah = label("yeah");
  labels.done = label("done");
  pcol = columns.begin();
}

cbl_label_t *
eval_subject_t::label( const char skel[] ) {
  static const cbl_label_t protolabel = { LblEvaluate };
  cbl_label_t label = protolabel;
  label.line = yylineno;
  size_t n = 1 + symbols_end() - symbols_begin();
  snprintf(label.name, sizeof(label.name), "_eval_%s_%zu", skel, n);
  auto output = symbol_label_add( PROGRAM, &label );
  return output;
}

bool
eval_subject_t::compatible( const cbl_field_t *object ) const {
  assert(pcol != columns.end());
  assert(pcol->field);
  auto subject(pcol->field);
  if( subject->type != object->type ) {
    if( is_conditional(subject) ) {
      return is_conditional(object);
    }
    return ! is_conditional(object);
  }
  return true;
}


cbl_field_t *
eval_subject_t::compare( int token ) {
  size_t tf( very_false_register() );

  switch( token ) {
  case ANY:
    parser_logop(result,
		 field_at(very_true_register()), and_op,
		 field_at(very_true_register()));
    break;
  case TRUE_kw:
    tf = very_true_register();
    __attribute__((fallthrough));
  case FALSE_kw:
    assert( is_conditional(pcol->field) );
    parser_logop(this->result, pcol->field, xnor_op, field_at(tf));
    break;
  default:
    assert(token == -1 && false );
    break;
  }
  return result;
}

cbl_field_t *
eval_subject_t::compare( relop_t op, const cbl_refer_t& object, bool deciding ) {
  auto subject(*pcol);
  if( compatible(object.field) ) {
    if( ! is_conditional(subject.field) ) {
      auto result = deciding? this->result : new_temporary(FldConditional);
      parser_relop(result, subject, op, object);
      return result;
      }
    }
  if( yydebug ) {
    dbgmsg("%s:%d: failed for %s %s %s",
	  __func__, __LINE__,
	  name_of(subject.field), relop_str(op), name_of(object.field));
  }
  return nullptr;
}

cbl_field_t *
eval_subject_t::compare( const cbl_refer_t& object,
			 const cbl_refer_t& object2 ) {
  auto subject(*pcol);

  if( ! compatible( object.field ) ) {
    if( yydebug ) {
      dbgmsg("%s:%d: failed for %s %s",
	    __func__, __LINE__,
	    name_of(subject.field), name_of(object.field));
    }
    return nullptr;
  }
  if( object2.field ) {
    if( ! compatible( object2.field ) ) {
      if( yydebug ) {
	dbgmsg("%s:%d: failed for %s %s",
	      __func__, __LINE__,
	      name_of(subject.field), name_of(object2.field));
      }
      return nullptr;
    }
  }

  if( is_conditional(subject.field) ) {
    assert( object2.field == nullptr );
    parser_logop(result, subject.field, xnor_op, object.field);
    return result;
  }

  if( object2.field ) {
    assert( ! is_conditional(object.field) );
    assert( ! is_conditional(object2.field) );

    cbl_field_t * gte = new_temporary(FldConditional);
    cbl_field_t * lte = new_temporary(FldConditional);

    parser_relop( gte, object, le_op, subject );
    parser_relop( lte, subject, le_op, object2 );

    parser_logop(result, gte, and_op, lte);
    return result;
  }

  parser_relop(result, subject, eq_op, object);
  return result;
}
