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

#include <assert.h>
#include <string.h>
#include <stdio.h>

#include <algorithm>
#include <list>
#include <map>
#include <numeric>
#include <stack>
#include <string>

#define MAXLENGTH_FORMATTED_DATE     10
#define MAXLENGTH_FORMATTED_TIME     19
#define MAXLENGTH_CALENDAR_DATE      21
#define MAXLENGTH_FORMATTED_DATETIME 30

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"

extern void declarative_runtime_match(cbl_field_t *declaratives,
                                      cbl_label_t *lave );

extern YYLTYPE yylloc;

extern int yylineno, yyleng, yychar;
extern char *yytext;

bool need_nume_set( bool tf = true );

bool max_errors_exceeded( int nerr );

extern cbl_label_t *next_sentence;
void next_sentence_label(cbl_label_t* label) {
  parser_label_label(label);
  next_sentence = NULL;
  // release codegen label structure, so it can be reused.
  assert(label->structs.goto_trees || mode_syntax_only());
  free(label->structs.goto_trees);
  label->structs.goto_trees = NULL;
}

void apply_declaratives();
const char * keyword_str( int token );
void labels_dump();

cbl_dialect_t cbl_dialect;
size_t cbl_gcobol_features;

static size_t nparse_error = 0;

size_t parse_error_inc() { return ++nparse_error; }
size_t parse_error_count() { return nparse_error; }
void input_file_status_notify();

#define YYLLOC_DEFAULT(Current, Rhs, N)					\
  do {									\
      if (N)								\
        {								\
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
          location_dump("parse.c", N,					\
                        "rhs N  ", YYRHSLOC (Rhs, N));			\
        }								\
      else								\
        {								\
          (Current).first_line   =					\
          (Current).last_line    = YYRHSLOC (Rhs, 0).last_line;		\
          (Current).first_column =					\
          (Current).last_column  = YYRHSLOC (Rhs, 0).last_column;	\
        }								\
      location_dump("parse.c", __LINE__, "current", (Current));		\
      gcc_location_set( location_set(Current) );			\
      input_file_status_notify();					\
  } while (0)

int yylex(void);
extern int yydebug;

#include <stdarg.h>

const char *
consistent_encoding_check( const YYLTYPE& loc, const char input[] ) {
  cbl_field_t faux = {};
  faux.type = FldAlphanumeric;
  faux.data.capacity = capacity_cast(strlen(input));
  faux.data.initial = input;

  auto s = faux.internalize();
  if( !s ) {
    error_msg(loc, "inconsistent string literal encoding for '%s'", input);
  } else {
    if( s != input ) return s;
  }
  return NULL;
}

const char * original_picture();
      char * original_number( char input[] = NULL );

static const relop_t invalid_relop = static_cast<relop_t>(-1);

static enum cbl_division_t current_division;

static cbl_refer_t null_reference;
static cbl_field_t *literally_one, *literally_zero;

cbl_field_t *
literal_of( size_t value ) {
  switch(value) {
    case 0: return literally_zero;
    case 1: return literally_one;
  }
  cbl_err("logic error: %s: %zu not supported", __func__, value);
  return NULL;
}

enum data_section_t { // values reflect mandatory order
  not_data_datasect_e,
  file_datasect_e,
  working_storage_datasect_e,
  local_storage_datasect_e,
  linkage_datasect_e,
} current_data_section;

static bool current_data_section_set( const YYLTYPE& loc, enum data_section_t );

enum data_clause_t {
  picture_clause_e     = 0x0001,
  usage_clause_e       = 0x0002,
  value_clause_e       = 0x0004,
  occurs_clause_e      = 0x0008,
  global_clause_e      = 0x0010,
  external_clause_e    = 0x0020,
  justified_clause_e   = 0x0040,
  redefines_clause_e   = 0x0080,
  blank_zero_clause_e  = 0x0100,
  synched_clause_e     = 0x0200,
  sign_clause_e        = 0x0400,
  based_clause_e       = 0x0800,
  same_clause_e        = 0x1000,
  volatile_clause_e    = 0x2000,
  type_clause_e        = 0x4000,
  typedef_clause_e     = 0x8000,
};

static inline bool
has_clause( int data_clauses, data_clause_t clause ) {
  return clause == (data_clauses & clause);
}

static bool
is_cobol_word( const char name[] ) {
  auto eoname = name + strlen(name);
  auto p = std::find_if( name, eoname,
                         []( char ch ) {
                           switch(ch) {
                           case '-':
                           case '_':
                             return false;
                           case '$': // maybe one day (IBM allows)
                             break;
                           }
                           return !ISALNUM(ch);
                         } );
  return p == eoname;
}

bool
in_procedure_division(void) {
  return current_division == procedure_div_e;
}

static inline bool
in_file_section(void) { return current_data_section == file_datasect_e; }

static cbl_refer_t *
intrinsic_inconsistent_parameter( size_t n, cbl_refer_t *args );

static inline bool
namcpy(const YYLTYPE& loc, cbl_name_t tgt, const char *src ) {
  // snprintf(3): writes at most size bytes (including the terminating NUL byte)
  auto len = snprintf(tgt, sizeof(cbl_name_t), "%s", src);
  if( ! (0 < len && len < int(sizeof(cbl_name_t))) ) {
    error_msg(loc, "name truncated to '%s' (max %zu characters)",
              tgt, sizeof(cbl_name_t)-1);
    return false;
  }
  return true;
}

cbl_field_t *
new_alphanumeric( size_t capacity = MAXIMUM_ALPHA_LENGTH );

static inline cbl_refer_t *
new_reference( enum cbl_field_type_t type, const char *initial ) {
  return new cbl_refer_t( new_temporary(type, initial) );
}
static inline cbl_refer_t *
new_reference( cbl_field_t *field ) {
  return new cbl_refer_t(field);
}
static inline cbl_refer_t *
new_reference_like( const cbl_field_t& skel ) {
  return new cbl_refer_t( new_temporary_like(skel) );
}

static void reject_refmod( YYLTYPE loc, cbl_refer_t );
static bool require_pointer( YYLTYPE loc, cbl_refer_t );
static bool require_numeric( YYLTYPE loc, cbl_refer_t );

struct cbl_field_t * constant_of( size_t isym );

static const struct cbl_occurs_t nonarray = cbl_occurs_t();

using std::list;

static inline bool isquote( char ch ) {
  return ch == '\'' || ch == '"';
}

static inline char * dequote( char input[] ) {
  char *pend = input + strlen(input) - 1;
  assert(isquote(*input));
  assert(isquote(*pend));
  assert(*input == *pend);
  *input = *pend = '\0';
  return ++input;
}

static const char *
name_of( cbl_field_t *field ) {
  assert(field);
  return field->name[0] == '_' && field->data.initial?
    field->data.initial : field->name;
}

static const char *
nice_name_of( cbl_field_t *field ) {
  auto name = name_of(field);
  return name[0] == '_'? "" : name;
}

struct evaluate_elem_t {
  size_t nother;
  struct cbl_label_t label;
  struct cbl_field_t *result;
  struct case_t {
   private:
    relop_t oper;
   public:
    cbl_field_t *subject, *object, *cond;
    case_t( cbl_field_t * subject )
      : oper(eq_op)
      , subject(subject)
      , object(NULL)
      , cond( keep_temporary(FldConditional) )
    {}

    cbl_field_t * object_set( cbl_field_t *obj, relop_t op ) {
      oper = op;
      return object = obj;
    }

    inline relop_t op() const { return oper; }

    void dump() const {
      dbgmsg( "   cond is '%s'\n\t"
             "subject is '%s'\n\t"
             "   oper is  %s \n\t"
             " object is '%s'",
             cond? xstrdup(field_str(cond)) : "none",
             subject? xstrdup(field_str(subject)) : "none",
             relop_str(oper),
             object? xstrdup(field_str(object)) : "none");
    }
    static void Dump( const case_t& c ) { c.dump(); }
  };
  list<case_t>   cases;
  typedef list<case_t>::iterator case_iter;
  case_iter pcase;

  void dump() const {
    dbgmsg( "nother=%zu label '%s', %zu cases", nother, label.name, cases.size() );
    std::for_each( cases.begin(), cases.end(), case_t::Dump );
  }

  explicit evaluate_elem_t( const char skel[] )
    : nother(0)
    , result( keep_temporary(FldConditional) )
    , pcase( cases.end() )
  {
    static const cbl_label_t protolabel = { LblEvaluate };
    label = protolabel;
    label.line = yylineno;
    if( -1 == snprintf(label.name, sizeof(label.name),
                       "%.*s_%d", (int)sizeof(label.name)-6, skel, yylineno) ) {
      yyerror("could not create unique label '%s_%d' because it is too long",
              skel, yylineno);
    }
  }

  size_t ncolumn() const { return cases.size(); }
  size_t nready() const {
    size_t n=0;
    for( const auto& c : cases ) {
      if( c.object == NULL ) break;
      n++;
    }
    return n;
  }
};

/*
 * The file_X_args variables hold the arguments to parser_file_X. The
 * X_body nonterminal collects the arguments, but we defer calling
 * parser_file_X until either:
 *  1.  end of statement, implying sequentiality, or
 *  2.  ON ERROR, implying random access
 * In the 2nd case, the call to parser_file_X is made at the top of
 * the io_error nonterminal, before any statements are parsed. The
 * effect is to delay the call only until we've parsed ON ERROR.
 * Because there are no intervening statements, there's no need for a
 * stack of arguments. One global does the trick.
*/
static class file_delete_args_t {
  cbl_file_t *file;
public:
  void init( cbl_file_t *file ) {
    this->file = file;
  }
  bool ready() const { return file != NULL; }
  void call_parser_file_delete( bool sequentially ) {
    parser_file_delete(file, sequentially);
    file = NULL;
  }
} file_delete_args;

cbl_round_t current_rounded_mode();

static struct file_read_args_t {
  cbl_file_t *file;
  cbl_refer_t record, *read_into;
  int where;
  enum { where_unknown = 0 };

  file_read_args_t() : file(NULL), read_into(NULL), where(where_unknown) {}

  void
  init( struct cbl_file_t *file,
        cbl_refer_t record,
        cbl_refer_t *read_into,
        int where ) {
    this->file = file;
    this->record = record;
    this->read_into = read_into;
    this->where = where;
  }

  bool ready() const { return file != NULL; }
  void default_march( bool sequential ) {
    if( where == where_unknown ) {
      where = sequential? -1 : 1;
    }
  }

  void
  call_parser_file_read( int w = where_unknown) {
    if( w != where_unknown ) where = w;
    if( where == where_unknown) {
      switch( file->access ) {
      case file_inaccessible_e:
      case file_access_seq_e:
        where = -1;
        break;
      case file_access_rnd_e:
        where = 1;
        break;
      case file_access_dyn_e:
        where = 1;
       break;
      }
    }
    parser_file_read(file, record, where);
    if( read_into ) {
      parser_move( *read_into, record, current_rounded_mode() );
    }
    *this = file_read_args_t();
  }
} file_read_args;

static class file_return_args_t {
  cbl_file_t *file;
public:
  file_return_args_t() : file(NULL) {}
  void init( cbl_file_t *file ) {
    this->file = file;
  }
  bool ready() const { return file != NULL; }
  void call_parser_return_start(cbl_refer_t into = cbl_refer_t() ) {
    parser_return_start(file, into);
    file = NULL;
  }
} file_return_args;

static class file_rewrite_args_t {
  cbl_file_t *file;
  cbl_field_t *record;
public:
  void init( cbl_file_t *file, cbl_field_t *record ) {
    this->file = file;
    this->record = record;
  }
  bool ready() const { return file != NULL; }
  void call_parser_file_rewrite( bool sequentially ) {
    sequentially = sequentially || file->access == file_access_seq_e;
    if( file->access == file_access_rnd_e ) sequentially = false;
    parser_file_rewrite(file, record, sequentially);
    file = NULL;
    record = NULL;
  }
} file_rewrite_args;

static class file_start_args_t {
  cbl_file_t *file;
public:
  file_start_args_t() : file(NULL) {}
  void init( YYLTYPE loc, cbl_file_t *file ) {
    this->file = file;
    if( is_sequential(file) ) {
      error_msg(loc, "START invalid with sequential file %s", file->name);
    }
  }
  bool ready() const { return file != NULL; }
  void call_parser_file_start() {
    // not needed: parser_file_start(file, sequentially);
    file = NULL;
  }
} file_start_args;

static class file_write_args_t {
  cbl_file_t *file;
  cbl_field_t *data_source;
  bool after;
  cbl_refer_t *advance;
public:
  file_write_args_t()
    : file(NULL)
    , after(false)
    , advance(NULL)
  {}
  cbl_file_t * init( cbl_file_t *file,
                     cbl_field_t *data_source,
                     bool after,
                     cbl_refer_t *advance ) {
    this->file = file;
    this->data_source = data_source;
    this->after = after;
    this->advance = new cbl_refer_t(*advance);
    return this->file;
  }
  bool ready() const { return file != NULL; }
  void call_parser_file_write( bool sequentially ) {
    sequentially = sequentially || file->access == file_access_seq_e;
    parser_file_write(file, data_source, after, *advance, sequentially);
    *this = file_write_args_t();
  }
} file_write_args;

/*
 * Fields
 */
struct group_attr_t {
  cbl_field_type_t default_usage; // for COMP-5 etc.
  int encoding;                   // for ASCII, National, etc.
  cbl_field_t *field;

  group_attr_t( cbl_field_t *field,
                cbl_field_type_t default_usage,
                int encoding )
    : default_usage(default_usage)
    , encoding(encoding)
    , field(field)
  {}
};

struct refer_list_t;

struct arith_t {
  cbl_arith_format_t format;
  list<cbl_num_result_t> tgts;
  list<cbl_refer_t> A, B;
  cbl_refer_t remainder;
  cbl_label_t *on_error, *not_error;

  arith_t( cbl_arith_format_t format )
    : format(format), on_error(NULL), not_error(NULL)
  {}
  arith_t( cbl_arith_format_t format, refer_list_t * refers );

  bool corresponding() const { return format == corresponding_e; }

  void another_pair( size_t src, size_t tgt ) {
    assert(src > 0 && tgt > 0);

    cbl_refer_t a(A.front());
    a.field = cbl_field_of(symbol_at(src));
    A.push_back( a );

    cbl_num_result_t res = tgts.front();
    res.refer.field = cbl_field_of(symbol_at(tgt));
    tgts.push_back( res );

    dbgmsg("%s:%d: SRC: %3zu %s", __func__, __LINE__, src, a.str());
    dbgmsg("%s:%d:   to %3zu %s", __func__, __LINE__, tgt, res.refer.str());
  }
  void operator()( const corresponding_fields_t::const_reference elem ) {
    another_pair( elem.first, elem.second );
  }

  const char * format_str() const {
    switch(format) {
    case not_expected_e:  return "not_expected_e";
    case no_giving_e:     return "no_giving_e";
    case giving_e:        return "giving_e";
    case corresponding_e: return "corresponding_e";
    }
    return "???";
  }
};

static cbl_refer_t * ast_op( cbl_refer_t *lhs, char op, cbl_refer_t *rhs );

static void ast_add( arith_t *arith );
static bool ast_subtract( arith_t *arith );
static bool ast_multiply( arith_t *arith );
static bool ast_divide( arith_t *arith );

static cbl_field_type_t intrinsic_return_type( int token );

template <typename T>
static T* use_any( list<T>& src, T *tgt) {
  if( src.empty() ) return NULL;

  std::copy(src.begin(), src.end(), tgt);
  src.clear();

  return tgt;
}
template <typename T>
static T* use_any( list<T>& src, std::vector<T>& tgt) {
  if( src.empty() ) return NULL;

  std::copy(src.begin(), src.end(), tgt.begin());
  src.clear();

  return tgt.data();
}

class evaluate_t;
/*
 * Evaluate
 */
class eval_subject_t {
  friend evaluate_t;
  struct { cbl_label_t *done, *yeah, *when; } labels;
  cbl_field_t *result;
  relop_t abbr_relop;
  typedef std::list<cbl_refer_t> column_list_t;
  column_list_t columns;
  column_list_t::iterator pcol;

  static cbl_label_t * label( const char skel[] );

  void new_object_labels();
 public:
  eval_subject_t();
  void append( cbl_refer_t field ) {
    columns.push_back(field);
    pcol = columns.begin();
  }
  cbl_label_t *yeah() { return labels.yeah; }
  cbl_label_t *when() { return labels.when; }
  cbl_label_t *done() { return labels.done; }

  cbl_field_t *subject() const {
    if( pcol == columns.end() ) return nullptr;
    return pcol->field;
  }
  size_t subject_count() const { return columns.size(); }
  size_t  object_count() { return  std::distance(columns.begin(), pcol); }

  void object_relop( relop_t op ) { abbr_relop = op; }
  relop_t object_relop() const { return abbr_relop; }

  void rewind() { pcol = columns.begin(); }

  bool compatible( const cbl_field_t *object ) const;

  // compare sets result
  cbl_field_t * compare( int token );
  cbl_field_t * compare( relop_t op,
                         const cbl_refer_t& object, bool deciding = false);
  cbl_field_t * compare( const cbl_refer_t& object,
                         const cbl_refer_t& object2 = nullptr);

  void write_when_label() {
    parser_label_label(labels.when);
    labels.when = label("when");
  }
  void write_yeah_label() {
    parser_label_label(labels.yeah);
    labels.yeah = label("yeah");
  }

  // decide() calls codegen with the result and increments the subject column.
  // On FALSE, skip past <statements> and fall into next WHEN.
  bool decided( cbl_field_t *result ) {
    this->result = result;
    parser_if( result );
    parser_else();
    parser_label_goto( labels.when );
    parser_fi();
    pcol++;
    return true;
  }
  bool decide( int token ) {
    if( pcol == columns.end() ) return false;
    if( compare( token ) ) {
      parser_if( result );
      parser_else();
      parser_label_goto( labels.when );
      parser_fi();
    }
    pcol++;
    return true;
  }
  bool decide( const cbl_refer_t& object, bool invert ) {
    if( pcol == columns.end() ) return false;
    if( compare( object ) ) {
      if( invert ) {
        parser_logop( result, NULL, not_op, result );
      }
      parser_if( result );
      parser_else();
      parser_label_goto( labels.when );
      parser_fi();
    }
    pcol++;
    return true;
  }
  bool decide( relop_t op, const cbl_refer_t& object, bool invert ) {
    if( pcol == columns.end() ) return false;
    dbgmsg("%s() if not %s goto %s", __func__, result->name, when()->name);
    
    if( compare(op, object, true) ) {
      if( invert ) {
        parser_logop( result, NULL, not_op, result );
      }
      parser_if( result );
      parser_else();
      parser_label_goto( labels.when );
      parser_fi();
    }
    pcol++;
    return true;
  }
  bool decide( const cbl_refer_t& object, const cbl_refer_t& object2, bool invert ) {
    if( pcol == columns.end() ) return false;
    if( compare(object, object2) ) {
      if( invert ) {
        parser_logop( result, NULL, not_op, result );
      }
      parser_if( result );
      parser_else();
      parser_label_goto( labels.when );
      parser_fi();
    }
    pcol++;
    return true;
  }
};

class evaluate_t : private std::stack<eval_subject_t> {
public:
  size_t depth() const { return size(); }

  void alloc() {
    push(eval_subject_t());
  }
  void free()  { assert(!empty()); pop(); }

  eval_subject_t&  current() {
    assert(!empty());
    if( yydebug ) {
      auto& ev( top() );
      dbgmsg("eval_subject: res: %s, When %s, Yeah %s, Done %s",
              ev.result->name,
              ev.when()->name, ev.yeah()->name, ev.done()->name);
    }
    return top();
  }

} eval_stack;



static void dump_inspect( const cbl_inspect_t& i );

struct perform_t {
  struct cbl_perform_tgt_t tgt;
  bool before;
  list<cbl_perform_vary_t> varys;
  list<cbl_declarative_t>  dcls;

  struct ec_labels_t {
    cbl_label_t
      *init,      // Format 3, code that installs handlers
      *fini,      // Format 3, code that reverts handlers
      *top,       // Format 3, above imperative-statement-1
      *from,      // Format 3, imperative-statement-1
      *finally,
      *other, *common;
    ec_labels_t()
      : init(NULL), fini(NULL),
        top(NULL), from(NULL), finally(NULL),
        other(NULL), common(NULL)
    {}
    void generate() {
      init    = new_label( LblLoop, "init" );
      fini    = new_label( LblLoop, "fini" );
      top     = new_label( LblLoop, "top"   );
      from    = new_label( LblLoop, "from" );
      other   = new_label( LblLoop, "other" );
      common  = new_label( LblLoop, "common" );
      finally = new_label( LblLoop, "finally" );
    }
    static cbl_label_t *
    new_label( cbl_label_type_t type, const cbl_name_t role );
  } ec_labels;

  struct {
    cbl_label_t *start, *end;
    cbl_field_t *unsatisfied, *size;
    cbl_refer_t table;
  } search;

  perform_t( cbl_label_t *from, cbl_label_t *to = NULL )
    : tgt( from, to ), before(true)
  {
    search = {};
  }
  ~perform_t() { varys.clear(); }
  cbl_field_t * until() {
    assert(!varys.empty());
    cbl_field_t *f = varys.front().until;
    assert(f->type == FldConditional);
    return f;
  }
};

static list<perform_t> performs;

static inline perform_t *
perform_alloc() {
  performs.push_back(perform_t(NULL));
  return &performs.back();
}

static inline void
perform_free(void) {
  assert(performs.size() > 0);
  performs.pop_back();
}

static inline perform_t *
perform_current(void) {
  assert(performs.size() > 0);
  return &performs.back();
}

static inline perform_t *
  perform_tgt_set( cbl_label_t *from, cbl_label_t *to = NULL ) {
  struct perform_t *perf = perform_current();
  perf->tgt = cbl_perform_tgt_t(from, to);
  return perf;
}

#define PERFORM_EXCEPT 1
static void
perform_ec_setup() {
  struct perform_t *perf = perform_current();
  perf->ec_labels.generate();
  perf->tgt.from( perf->ec_labels.from );

#if PERFORM_EXCEPT
  parser_label_goto(perf->ec_labels.init);
  parser_label_label(perf->ec_labels.top);
#endif
  parser_perform_start(&perf->tgt);
}

static void
perform_ec_cleanup() {
  struct perform_t *perf = perform_current();
#if PERFORM_EXCEPT
  parser_label_goto(perf->ec_labels.fini);
  parser_label_label(perf->ec_labels.init);
      /* ... empty init block ... */
  parser_label_goto(perf->ec_labels.top);
  parser_label_label(perf->ec_labels.fini);
#endif
}

static list<cbl_label_t*> searches;

static inline cbl_label_t *
search_alloc( cbl_label_t *name ) {
  searches.push_back(name);
  return searches.back();
}

static inline void
search_free(void) {
  assert(searches.size() > 0);
  searches.pop_back();
}

static inline cbl_label_t *
search_current(void) {
  assert(searches.size() > 0);
  return searches.back();
}

static  list<cbl_num_result_t> rhs;
typedef list<cbl_num_result_t>::iterator rhs_iter;

struct tgt_list_t {
  list<cbl_num_result_t> targets;
};

static struct cbl_label_t *
label_add( const YYLTYPE& loc, enum cbl_label_type_t type, const char name[] );
static struct cbl_label_t *
label_add( enum cbl_label_type_t type, const char name[], int line );

static struct cbl_label_t *
paragraph_reference( const char name[], size_t section );

static inline void
list_add( list<cbl_num_result_t>& list, cbl_refer_t refer, int round ) {
  struct cbl_num_result_t arg = { static_cast<cbl_round_t>(round), refer };
  list.push_back(arg);
}

static  list<cbl_domain_t> domains;
typedef list<cbl_domain_t>::iterator domain_iter;

/*
 * The name queue is a queue of lists of data-item names recognized by the
 * lexer, but not returned to the parser.  These lists are "teed up" by the
 * lexer until no more qualifiers are found.  At that point, the last name is
 * returned as a NAME or NAME88 token. NAME88 is returned only if a correctly,
 * uniquely specified Level 88 data item is found in the symbol table (because
 * else we can't know).
 *
 * When the parser gets a NAME or NAME88 token, it retrieves the pending list
 * of qualifiers, if any, from the name queue.  It adds the returned name to
 * the list and calls symbol_find() to search the name map.  For correctly
 * specified names, the lexer has already done that work, which is now
 * unfortunately repeated.  For incorrect names, the parser emits a most useful
 * diagnostic.
 */
static  name_queue_t name_queue;

void
tee_up_empty() {
  name_queue.allocate();
}
void
tee_up_name( const YYLTYPE& loc, const char name[] ) {
  name_queue.push(loc, name);
}
cbl_namelist_t
teed_up_names() {
  return name_queue_t::namelist_of( name_queue.peek() );
}

class tokenset_t {
  std::vector<const char *>token_names;
  std::map <std::string, int> tokens;
  std::set<std::string> cobol_words;

  static std::string
  lowercase( const cbl_name_t name ) {
    cbl_name_t lname;
    std::transform(name, name + strlen(name) + 1, lname, ftolower);
    return lname;
  }

 public:
  tokenset_t();
  int find( const cbl_name_t name, bool include_intrinsics );

  bool equate( const YYLTYPE& loc, int token, const cbl_name_t name ) {
    auto lname( lowercase(name) );
    auto cw = cobol_words.insert(lname);
    if( ! cw.second ) {
      error_msg(loc, "COBOL-WORDS EQUATE: %s may appear but once", name);
      return false;
    }
    auto p = tokens.find(lowercase(name));
    bool fOK = p == tokens.end();
    if( fOK ) { // name not already in use
      tokens[lname] = token;
    } else {
      error_msg(loc, "EQUATE: %s already defined as a token", name);
    }
    return fOK;
  }
  bool undefine( const YYLTYPE& loc, const cbl_name_t name ) {
    auto lname( lowercase(name) );
    auto cw = cobol_words.insert(lname);
    if( ! cw.second ) {
      error_msg(loc, "COBOL-WORDS UNDEFINE: %s may appear but once", name);
      return false;
    }
    auto p = tokens.find(lname);
    bool fOK = p != tokens.end();
    if( fOK ) { // name in use
      tokens.erase(p);
    } else {
      error_msg(loc, "UNDEFINE: %s not defined as a token", name);
    }
    return fOK;
  }
  bool substitute( const YYLTYPE& loc, const cbl_name_t extant, int token, const cbl_name_t name ) {
    return equate( loc, token, name ) && undefine( loc, extant );
  }
  bool reserve( const YYLTYPE& loc, const cbl_name_t name ) {
    auto lname( lowercase(name) );
    auto cw = cobol_words.insert(lname);
    if( ! cw.second ) {
      error_msg(loc, "COBOL-WORDS RESERVE: %s may appear but once", name);
      return false;
    }
    tokens[lname] = -42;
    return true;
  }
  int redefined_as( const cbl_name_t name ) {
    auto lname( lowercase(name) );
    if( cobol_words.find(lname) != cobol_words.end() ) {
      auto p = tokens.find(lname);
      if( p != tokens.end() ) {
        return p->second;
      }
    }
    return 0;
  }
  const char * name_of( int tok ) const {
    tok -= (255 + 3);
    gcc_assert(0 <= tok && size_t(tok) < token_names.size());
    return token_names[tok];
  }
};

class current_tokens_t {
  tokenset_t tokens;
 public:
  current_tokens_t() {}
  int find( const cbl_name_t name, bool include_intrinsics ) {
    return tokens.find(name, include_intrinsics);
  }
  bool equate( const YYLTYPE& loc, cbl_name_t keyword, const cbl_name_t name ) {
    int token = keyword_tok(keyword);
    if( 0 == token ) {
      error_msg(loc, "EQUATE %s: not a valid token", keyword);
      return false;
    }
    return tokens.equate(loc, token, name);
  }
  bool undefine( const YYLTYPE& loc, cbl_name_t keyword ) {
    return tokens.undefine(loc, keyword);
  }
  bool substitute( const YYLTYPE& loc, cbl_name_t keyword, const cbl_name_t name ) {
    int token = keyword_tok(keyword);
    if( 0 == token ) {
      error_msg(loc, "SUBSTITUTE %s: not a valid token", keyword);
      return false;
    }
    return tokens.substitute(loc, keyword, token, name);
  }
  bool reserve( const YYLTYPE& loc, const cbl_name_t name ) {
    return tokens.reserve(loc, name);
  }
  int redefined_as( const cbl_name_t name ) {
    return tokens.redefined_as(name);
  }
  const char * name_of( int tok ) const {
    return tokens.name_of(tok);
  }
} tokens;

int
redefined_token( const cbl_name_t name ) {
  return tokens.redefined_as(name);
}

struct file_list_t {
  list<cbl_file_t*> files;
  file_list_t() {}
  file_list_t( cbl_file_t* file ) {
    files.push_back(file);
  }
  file_list_t( file_list_t& that ) : files(that.files.size()) {
    std::copy( that.files.begin(), that.files.end(), files.begin() );
  }

  static size_t symbol_index( cbl_file_t* file ) {
    return ::symbol_index( symbol_elem_of(file) );
  }
};

struct field_list_t {
  list<cbl_field_t*> fields;
  field_list_t( cbl_field_t *field ) {
    fields.push_back(field);
  }
  explicit field_list_t() {}
};

cbl_field_t **
use_list( field_list_t *src, cbl_field_t *tgt[] ) {
  assert(src);
  std::copy(src->fields.begin(), src->fields.end(), tgt);
  src->fields.clear();
  delete src;

  return tgt;
}

cbl_file_t **
  use_list( list<cbl_file_t*>& src, bool clear = true ) {
  if( src.empty() ) return NULL;
  auto tgt = new cbl_file_t*[ src.size() ];
  std::copy(src.begin(), src.end(), tgt);

  if( clear )
    src.clear();

  return tgt;
}

struct refer_list_t {
  list<cbl_refer_t> refers;
  refer_list_t( cbl_refer_t *refer ) {
    if( refer ) {
      refers.push_back(*refer);
      delete refer;
    }
  }
  refer_list_t * push_back( cbl_refer_t *refer ) {
    refers.push_back(*refer);
    delete refer;
    return this;
  }
  inline list<cbl_refer_t>& items() { return  refers; }
  inline list<cbl_refer_t>::iterator begin() { return  refers.begin(); }
  inline list<cbl_refer_t>::iterator end()   { return  refers.end(); }
  inline size_t size() const { return refers.size(); }

  cbl_refer_t *
  use_list( cbl_refer_t tgt[] ) {
    std::copy(refers.begin(), refers.end(), tgt);
    refers.clear();
    return tgt;
  }
};

struct refer_marked_list_t : public refer_list_t {
  cbl_refer_t *marker;

  refer_marked_list_t()  : refer_list_t(NULL),  marker(NULL) {}
  refer_marked_list_t( cbl_refer_t *marker, refer_list_t *refers )
    : refer_list_t(*refers), marker(marker) {}
  refer_marked_list_t( cbl_refer_t *marker, cbl_refer_t *input )
    : refer_list_t(input)
    , marker(marker) {}

  refer_marked_list_t * push_back( refer_list_t *refers ) {
    push_back(refers);
    return this;
  }
  refer_marked_list_t * push_on( cbl_refer_t *marker, cbl_refer_t *input ) {
    refers.push_back(*input);
    this->marker = marker;
    return this;
  }
};

struct refer_collection_t {
  list<refer_marked_list_t> lists;

  refer_collection_t( const refer_marked_list_t& marked_list )
  {
    lists.push_back( marked_list );
  }
  refer_collection_t * push_back( const refer_marked_list_t& marked_list )
  {
    lists.push_back( marked_list );
    return this;
  }

  const cbl_refer_t* last_delimiter() const {
    return lists.back().marker;
  }
  cbl_refer_t* last_delimiter( cbl_refer_t* marker) {
    return lists.back().marker = marker;
  }

  size_t total_size() const {
    size_t n = 0;
    for( auto p=lists.begin(); p != lists.end(); p++ ) {
      n += p->refers.size();
    }
    return n;
  }
};

struct ast_inspect_oper_t {
  cbl_inspect_bound_t bound;  // CHARACTERS/ALL/LEADING/FIRST
  std::list<cbl_inspect_match_t>    matches;
  std::list<cbl_inspect_replace_t> replaces;

ast_inspect_oper_t( const cbl_inspect_match_t& match,
                    cbl_inspect_bound_t bound = bound_characters_e )
    : bound(bound)
  {
    matches.push_back(match);
  }
  ast_inspect_oper_t( const cbl_inspect_replace_t& replace,
                    cbl_inspect_bound_t bound = bound_characters_e )
    : bound(bound)
  {
    replaces.push_back(replace);
  }
};

struct ast_inspect_t : public std::list<cbl_inspect_oper_t> {
  cbl_refer_t tally; // field is NULL for REPLACING
  const std::list<cbl_inspect_oper_t>& opers() const { return *this; }
};

struct ast_inspect_list_t : public std::list<cbl_inspect_t> {
  ast_inspect_list_t( const cbl_inspect_t& insp ) {
    push_back(insp);
  }

  cbl_inspect_t * as_array() {
    cbl_inspect_t *output = new cbl_inspect_t[ size() ];
    std::copy( begin(), end(), output );
    return output;
  }
};

void ast_inspect( cbl_refer_t& input, bool backward, ast_inspect_list_t& inspects );

template <typename E>
struct elem_list_t {
  list<E*> elems;
  elem_list_t( E *elem ) {
    elems.push_back(elem);
  }
  void clear() {
    for( auto p = elems.begin(); p != elems.add(); p++ ) {
      delete *p;
    }
    elems.clear();
  }
};

typedef elem_list_t<cbl_label_t> label_list_t;

template <typename L, typename E>
  E use_list( L *src, E tgt ) {
  assert(src);
  std::copy(src->elems.begin(), src->elems.end(), tgt);
  src->elems.clear();
  delete src;

  return tgt;
}

struct unstring_tgt_t {
  cbl_refer_t *tgt, *delimiter, *count;
  unstring_tgt_t( cbl_refer_t *tgt,
                  cbl_refer_t *delimiter = NULL,
                  cbl_refer_t *count = NULL )
    : tgt(tgt), delimiter(delimiter), count(count)
  {}

  static cbl_refer_t tgt_of( const unstring_tgt_t& that ) {
    return maybe_empty(that.tgt);
  }
  static cbl_refer_t delimiter_of( const unstring_tgt_t& that ) {
    return maybe_empty(that.delimiter);
  }
  static cbl_refer_t count_of( const unstring_tgt_t& that ) {
    return maybe_empty(that.count);
  }
private:
  static cbl_refer_t maybe_empty( cbl_refer_t *p ) {
    return p? *p : cbl_refer_t();
  }
};

struct unstring_tgt_list_t {
  list<unstring_tgt_t> unstring_tgts;

  unstring_tgt_list_t( unstring_tgt_t *unstring_tgt ) {
    unstring_tgts.push_back(*unstring_tgt);
    delete unstring_tgt;
  }
  unstring_tgt_list_t * push_back( unstring_tgt_t *unstring_tgt ) {
    unstring_tgts.push_back(*unstring_tgt);
    delete unstring_tgt;
    return this;
  }

  size_t size() const { return unstring_tgts.size(); }

  typedef cbl_refer_t xform_t( const unstring_tgt_t& that );
  void use_list( std::vector<cbl_refer_t>& output, xform_t func ) {
    std::transform( unstring_tgts.begin(),
                    unstring_tgts.end(),
                    output.begin(), func );
  }
};

struct unstring_into_t : public unstring_tgt_list_t {
  cbl_refer_t pointer, tally;
  unstring_into_t( unstring_tgt_list_t *tgt_list,
                   cbl_refer_t *pointer = NULL,
                   cbl_refer_t *tally = NULL )
    : unstring_tgt_list_t(*tgt_list)
    , pointer( pointer? *pointer : cbl_refer_t() )
    , tally( tally? *tally : cbl_refer_t() )
  {
    delete tgt_list;
    if( pointer ) delete pointer;
    if( tally ) delete tally;
  }
};

struct ffi_args_t {
  list<cbl_ffi_arg_t> elems;

  ffi_args_t( cbl_ffi_arg_t *arg ) {
    this->push_back(arg);
  }

  ffi_args_t( size_t narg, cbl_ffi_arg_t *args ) {
    std::copy(args, args+narg, std::back_inserter(elems));
  }

  // set explicitly, or assume
  ffi_args_t * push_back( cbl_ffi_arg_t *arg ) {
    if( arg->crv == by_default_e ) {
      arg->crv = elems.empty()? by_reference_e : elems.back().crv;
    }
    elems.push_back(*arg);
    delete arg;
    return this;
  }

  // infer reference/content/value from previous
  ffi_args_t * push_back( cbl_refer_t* refer,
                          cbl_ffi_arg_attr_t attr = none_of_e ) {
    cbl_ffi_crv_t crv = elems.empty()? by_reference_e : elems.back().crv;
    cbl_ffi_arg_t arg( crv, refer, attr );
    elems.push_back(arg);
    return this;
  }
  void dump() const {
    int i=0;
    for( const auto& arg : elems ) {
      dbgmsg( "%8d) %-10s %-16s %s", i++,
              cbl_ffi_crv_str(arg.crv),
              3 + cbl_field_type_str(arg.refer.field->type),
              arg.refer.field->pretty_name() );
    }
  }

  const char *
  parameter_types() const {
    auto output = new char[ 1 + elems.size() ];
    auto p = std::transform( elems.begin(), elems.end(), output,
                             []( auto arg ) {
                               return function_descr_t::parameter_type(*arg.field());
                             } );
    assert(output < p);
    p[-1] = '\0';
    return output;
  }
};

struct relop_abbr_t {
  relop_t relop;
  cbl_refer_t *rhs;
};

typedef struct elem_list_t<relop_abbr_t> relop_abbr__list_t;

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wreorder"

struct sort_key_t : public field_list_t {
  bool ascending;
  sort_key_t( bool ascending, field_list_t key )
    : ascending(ascending), field_list_t(key)
  {}
};

#pragma GCC diagnostic pop

struct sort_keys_t {
  list<sort_key_t> key_list;
};

struct file_sort_io_t {
  file_list_t file_list;
  cbl_perform_tgt_t tgt;

  file_sort_io_t( file_list_t& files ) : file_list(files) {}
  file_sort_io_t( cbl_perform_tgt_t& tgt ) : tgt(tgt.from(), tgt.to()) {}
  size_t nfile() const { return file_list.files.size(); }
};


struct merge_t {
  cbl_file_t *master;
  list<cbl_file_t*> updates;
  // collation missing
  enum output_type_t { output_unknown_e,
                       output_proc_e,
                       output_file_e } type;
  cbl_perform_tgt_t tgt;
  list<cbl_file_t*> outputs;

  merge_t( cbl_file_t *input ) : master(input), type(output_unknown_e) {}
};

static list<merge_t> merges;

static inline merge_t&
merge_alloc( cbl_file_t *file ) {
  merges.push_back(file);
  return merges.back();
}

static inline void
merge_free(void) {
  assert(merges.size() > 0);
  merges.pop_back();
}

static inline merge_t&
merge_current(void) {
  assert(merges.size() > 0);
  return merges.back();
}

static  list<cbl_refer_t> lhs;

struct vargs_t {
  std::list<cbl_refer_t> args;
    vargs_t() {}
    vargs_t( struct cbl_refer_t *p ) { args.push_back(*p); delete p; }
    void push_back( cbl_refer_t *p ) { args.push_back(*p); delete p; }
};

static const char intermediate[] = ":intermediate";

#include <set>

std::set<const char *> pristine_values;

// key is a name after DEBUGGING/ERROR/EXCEPTION
// value is the list of sections invoked
std::map<std::string, std::list<std::string>>
  debugging_clients, error_clients, exception_clients;

class prog_descr_t {
  std::set<std::string> call_targets, subprograms;
 public:
  std::set<function_descr_t> function_repository;
  size_t program_index, declaratives_index;
  cbl_label_t *declaratives_eval, *paragraph, *section;
  const char *collating_sequence;
  struct locale_t {
    cbl_name_t name; const char *os_name;
    locale_t(const cbl_name_t name = NULL, const char *os_name = NULL)
      : name(""), os_name(os_name) {
      if( name ) {
        bool ok = namcpy(YYLTYPE(), this->name, name);
        gcc_assert(ok);
      }
    }
  } locale;
  cbl_call_convention_t call_convention;
  cbl_options_t options;

  prog_descr_t( size_t isymbol )
    : program_index(isymbol)
    , declaratives_index(0)
    , declaratives_eval(NULL)
    , paragraph(NULL)
    , section(NULL)
    , collating_sequence(NULL)
  {
    call_convention = current_call_convention();
  }

 std::set<std::string> external_targets() {
   std::set<std::string> externals;
   std::set_difference( call_targets.begin(), call_targets.end(),
                        subprograms.begin(), subprograms.end(),
                        std::inserter(externals, externals.begin()) );
   return externals;
 }
};

static char *
uniq_label_impl( const char stem[], int line ) {
  char *name = xasprintf("%s_%d_%d", stem, yylineno, line);
  return name;
}
#define uniq_label(S) uniq_label_impl( (S), __LINE__ )

/*
 * One of these days, paragraph and section will have to move into
 * prog_descr_t, because the current section and paragraph depend on the
 * current program, which may be nested and "pop back" into existence at END
 * PROGRAM.
 */
struct error_labels_t {
  cbl_label_t *on_error, *not_error, *compute_error;
  error_labels_t() : on_error(NULL), not_error(NULL), compute_error(NULL) {}
  void clear() { on_error = not_error = compute_error = NULL; }
  error_labels_t& generate() {
    on_error =  label_add(LblArith, uniq_label("arith"), yylineno);
    not_error = label_add(LblArith, uniq_label("arith"), yylineno);
    compute_error = label_add(LblCompute, uniq_label("compute"), yylineno);
    return *this;
  }
};

struct cbl_typedef_less {
  bool operator()( const cbl_field_t *a, const cbl_field_t  *b ) const {
    auto result = strcasecmp(a->name, b->name);
    if( result < 0 ) return true;
    if( result > 0 ) return false;

    // Names that match are different if they're in different programs
    // and neither is external.
    auto lhs = field_index(a);
    auto rhs = field_index(b);
    if( lhs != rhs ) {
      if( !a->has_attr(external_e) && !b->has_attr(external_e) ) {
        return lhs < rhs;
      }
    }
    return false;
  }
};

static bool
is_conditional( const cbl_field_t *field ) {
  return FldConditional == field->type;
}
static bool
is_conditional( const cbl_refer_t *refer ) {
  return is_conditional(refer->field);
}

typedef std::set< const cbl_field_t*, cbl_typedef_less > unique_typedefs_t;

static cbl_label_t * implicit_paragraph();
static cbl_label_t *  implicit_section();

/*
 * Incomplete because not needed at this time: we do not attempt to
 * set used/lain for labels used by these functions:
 *    parser_lsearch_start(   cbl_label_t *name,
 *    parser_lsearch_conditional(cbl_label_t * name)
 *    parser_lsearch_when( cbl_label_t *name, cbl_field_t *conditional )
 *    parser_lsearch_end( cbl_label_t *name )
 *    parser_bsearch_start(   cbl_label_t* name,
 *    parser_bsearch_conditional( cbl_label_t* name )
 *    parser_bsearch_when(cbl_label_t* name,
 *    parser_bsearch_end( cbl_label_t* name )
 *    parser_string_overflow( cbl_label_t *name )
 *    parser_string_overflow_end( cbl_label_t *name )
 *    parser_call_exception( cbl_label_t *name )
 *    parser_call_exception_end( cbl_label_t *name )
 *    parser_entry_activate( size_t iprog, const cbl_label_t *declarative )
 */

class program_stack_t : protected  std::stack<prog_descr_t> {
  struct pending_t {
    cbl_call_convention_t call_convention;
    bool initial;
    pending_t()
      : call_convention(cbl_call_convention_t(0))
      , initial(false)
    {}
  } pending;
 public:
  cbl_call_convention_t
  pending_call_convention( cbl_call_convention_t convention ) {
    return pending.call_convention = convention;
  }
  bool pending_initial() { return pending.initial = true; }

  void push( prog_descr_t descr ) {
    cbl_call_convention_t current_call_convention = cbl_call_cobol_e;
    if( !empty() ) current_call_convention = top().call_convention;
    descr.call_convention = current_call_convention;
    std::stack<prog_descr_t>& me(*this);
    me.push(descr);
  }
  inline void pop() {
    std::stack<prog_descr_t>& me(*this);
    me.pop();
  }
  inline prog_descr_t& top() {
    std::stack<prog_descr_t>& me(*this);
    return me.top();
  }
  inline const prog_descr_t& top() const {
    const std::stack<prog_descr_t>& me(*this);
    return me.top();
  }
  inline size_t size() const {
    const std::stack<prog_descr_t>& me(*this);
    return me.size();
  }
  inline bool empty() const {
    const std::stack<prog_descr_t>& me(*this);
    return me.empty();
  }

  void apply_pending() {
    if( size() == 1 && 0 != pending.call_convention ) {
      top().call_convention = pending.call_convention;
  }
    if( pending.initial ) {
      auto e = symbol_at(top().program_index);
      auto prog(cbl_label_of(e));
      prog->initial = pending.initial;
    }
  }

  cbl_label_t *first_declarative() {
    auto eval = top().declaratives_eval;
    if( eval ) return eval;
    // scan stack container for declaratives
    for( auto& prog : c ) {
      if( prog.declaratives_eval ) {
        eval = prog.declaratives_eval;
        break;
      }
    }
    return eval;
  }
};

struct rel_part_t {
  cbl_refer_t *operand; // lhs
  bool has_relop, invert;
  relop_t relop;

  rel_part_t( cbl_refer_t *operand = NULL,
              relop_t relop = relop_t(-1),
              bool invert = false )
    : operand(operand),
      has_relop(relop != -1),
      invert(invert),
      relop(relop)
  {}
  rel_part_t& relop_set( relop_t op ) {
    has_relop = true;
    relop = op;
    return *this;
  }

  bool is_value() const { return operand && is_elementary(operand->field->type); }
};

/*
 * Evaluation of OR is deferred in case it's followed by AND.  As each
 * logical operand is encountered, it's first assigned to the
 * "andable" member.  As ANDs are encountered, they're ANDed to
 * andable.  When OR is first encountered, we've reached the end of a
 * string of ANDs (possibly empty): we move andable to orable, and
 * assign the rhs to andable (because it could be followed by AND).
 * Successive ORs produce (orable = orable OR andable), followed by
 * assigning the rhs to andable.
 *
 * At the end of the AND/OR evaluation, there is always an andable
 * value, because that's where we began.  If there is a orable, that
 * indicates that the final OR remains unevaluated.  In the resolve()
 * method, we OR the two, and return that orable.  If there's no
 * orable, we simply return the andable.
*/
class log_expr_t {
  cbl_field_t *orable, *andable;
 public:
  log_expr_t( cbl_field_t *init ) : orable(NULL), andable(init) {
    if( ! is_conditional(init) ) {
      dbgmsg("%s:%d: logic error: %s is not a truth value",
               __func__, __LINE__, name_of(init));
    }
  }

  cbl_field_t * and_term() {
    return andable;
  }
  log_expr_t * and_term( cbl_field_t *rhs ) {
    if( ! is_conditional(rhs) ) {
      dbgmsg("%s:%d: logic error: %s is not a truth value",
               __func__, __LINE__, name_of(rhs));
    } else {
      parser_logop( andable, andable, and_op, rhs );
    }
    return this;
  }
  log_expr_t * or_term( cbl_field_t *rhs ) {
    if( ! is_conditional(rhs) ) {
      dbgmsg("%s:%d: logic error: %s is not a truth value",
               __func__, __LINE__, name_of(rhs));
      return this;
    }
    if( ! orable ) {
      orable = andable;
    } else {
      parser_logop( orable, orable, or_op, andable );
    }
    andable = rhs;
    return this;
  }
  cbl_field_t * resolve() {
    assert(andable);
    if( orable ) {
      parser_logop( andable, orable, or_op, andable );
      orable = NULL;
    }
    assert(!orable);
    return andable; // leave in (initial) ANDable state
  }
  bool unresolved() const {
    return orable != NULL;
  }
};

static void ast_enter_section( cbl_label_t * );
static void ast_enter_paragraph( cbl_label_t * );

static class current_t {
  friend cbl_options_t current_options();
  cbl_options_t options_paragraph;
  program_stack_t programs;
  unique_typedefs_t typedefs;
  std::set<function_descr_t> udfs;
  int first_statement;
  bool in_declaratives;
  // from command line or early TURN
  std::list<cbl_exception_files_t> cobol_exceptions;

  error_labels_t error_labels;

  static void declarative_execute( cbl_label_t *eval ) {
    if( !eval ) {
      if( !enabled_exceptions.empty() ) {
        auto index = new_temporary(FldNumericBin5);
        parser_match_exception(index, NULL);
      }
      return;
    }
    assert(eval);
    auto iprog = symbol_elem_of(eval)->program;
    if( iprog  == current_program_index() ) {
      parser_perform(eval);
    } else {
      parser_entry_activate( iprog, eval );
      auto name = cbl_label_of(symbol_at(iprog))->name;
      cbl_unimplemented("Global declarative %s for %s",
                        eval->name, name);
      parser_call( new_literal(strlen(name), name, quoted_e),
                   cbl_refer_t(), 0, NULL, NULL, NULL, false );
    }
  }

  rel_part_t antecedent_cache;

 public:
  current_t()
    : first_statement(0)
    , in_declaratives(false)
    {}

  bool option( cbl_options_t::arith_t option ) {
    if( programs.size() == 1 ) {
      options_paragraph.arith = option;
      return true;
    }
    return false;
  }
  bool option_binary( cbl_options_t::float_endidanism_t option ) {
    if( programs.size() == 1 ) {
      options_paragraph.binary_endidanism = option;
      return true;
    }
    return false;
  }
  bool option_decimal( cbl_options_t::float_endidanism_t option ) {
    if( programs.size() == 1 ) {
      options_paragraph.decimal_endidanism = option;
      return true;
    }
    return false;
  }
  bool option( cbl_options_t::float_encoding_t option ) {
    if( programs.size() == 1 ) {
      options_paragraph.float_encoding = option;
      return true;
    }
    return false;
  }
  bool default_round( cbl_round_t option ) {
    if( programs.size() == 1 ) {
      options_paragraph.default_round = option;
      return true;
    }
    return false;
  }
  bool intermediate_round( cbl_round_t option ) {
    if( programs.size() == 1 ) {
      options_paragraph.intermediate_round = option;
      return true;
    }
    return false;
  }

  template <typename T>
  bool initial_option( cbl_section_type_t section, T value ) {
    if( programs.size() == 1 ) {
      switch( section ) {
      case file_sect_e:
      case linkage_sect_e:
        break;
      case working_sect_e:
        options_paragraph.initial_value.working = value;
        return true;
        break;
      case local_sect_e:
        options_paragraph.initial_value.local = value;
        return true;
        break;
      }
    }
    return false;
  }

  bool initial_value( cbl_section_type_t section, size_t isym ) {
    return initial_option( section, isym );
  }

  cbl_enabled_exceptions_t enabled_exception_cache;

  typedef std::list<cbl_declarative_t> declaratives_list_t;
  class declaratives_t : protected declaratives_list_t {
    struct file_exception_t {
      ec_type_t type; uint32_t file;
      bool operator<( const file_exception_t& that ) const {
        if( type == that.type ) return file < that.file;
        return type < that.type;
      }
    };
    std::set<file_exception_t> file_exceptions;
   public:
    bool empty() const {
      return declaratives_list_t::empty();
    }
    inline const declaratives_list_t& as_list() const { return *this; }

    bool add( const_reference declarative ) {
      auto d = std::find_if( begin(), end(),
                             [sect = declarative.section]( const_reference decl ) {
                               return decl.section == sect;
                             } );
      if( d != end() ) {
        auto label = cbl_label_of(symbol_at(d->section));
        yyerror("USE already defined for %s", label->name);
        return false;
      }
      for( auto f = declarative.files;
           f && f < declarative.files + declarative.nfile; f++ ) {
        file_exception_t ex = { declarative.type, *f };
        auto result = file_exceptions.insert(ex);
        if( ! result.second ) {
          yyerror("%s defined twice for %s",
                   ec_type_str(declarative.type),
                   cbl_file_of(symbol_at(*f))->name);
          return false;
        }
      }
      declaratives_list_t::push_back(declarative);
      return true;
    }
  } declaratives;

  void exception_add( ec_type_t ec,  bool enabled = true) {
    std::set<size_t> files;
    enabled_exceptions.turn_on_off(enabled,
                                   false,  // for now
                                   ec, files);
    if( yydebug) enabled_exceptions.dump();
  }

  bool typedef_add( const cbl_field_t *field ) {
    auto result = typedefs.insert(field);
    return result.second;
  }
  const cbl_field_t * has_typedef( const cbl_field_t *field ) {
    auto found = typedefs.find(field);
    return found == typedefs.end()? NULL : *found;
    return found == typedefs.end()? NULL : *found;
  }

  void udf_add( size_t isym ) {
    auto udf = function_descr_t::init(isym);
    auto p = udfs.insert(udf);
    assert(p.second);
  }
  const function_descr_t * udf_in( const char name[] ) {
    auto udf = function_descr_t::init(name);
    auto p = udfs.find(udf);
    const function_descr_t *output = NULL;
    if( p != udfs.end() ) output =  &*p;
    return output;
  }
  void udf_update( const ffi_args_t *ffi_args );
  bool udf_args_valid( const cbl_label_t *func,
                       const std::list<cbl_refer_t>& args,
                       std::vector<function_descr_arg_t>& params /*out*/ );

  void udf_dump() const {
    if( yydebug ) {
      int i=0;
      for( auto udf : udfs ) {
        dbgmsg("%4d %-30s %-30s", i++, keyword_str(udf.token), udf.name);
      }
    }
  }

  void repository_add_all();
  bool repository_add( const char name[] );
  int  repository_in( const char name[] );

  bool repository_add( size_t isym ) {
    auto udf = function_descr_t::init(isym);
    auto p = udfs.find(udf); // previously defined functions in "udfs"
    assert(p != udfs.end());  // If it's a symbol, it must be in udfs.
    auto result = programs.top().function_repository.insert(*p);
    if( yydebug ) {
      for( auto descr : programs.top().function_repository ) {
        dbgmsg("%s:%d: %-20s %-20s %-20s", __func__, __LINE__,
              keyword_str(descr.token), descr.name, descr.cname);
      }
    }
    return result.second;
  }

  size_t declarative_section() const {
    return symbol_index(symbol_elem_of(programs.top().section));
  }
  const char * declarative_section_name() const {
    return in_declaratives? programs.top().section->name : NULL;
  }

  std::list<std::string>& debugging_declaratives(bool all) const {
    const char *para = programs.top().paragraph->name;
    auto declaratives = debugging_clients.find(all? ":all:" : para);
    if( declaratives == debugging_clients.end() ) {
      static std::list<std::string> empty;
      return empty;
    }
    return declaratives->second;
  }

  bool
  collating_sequence( const cbl_name_t name ) {
    assert(name);
    assert(!programs.empty());
    prog_descr_t& program = programs.top();
    if( program.collating_sequence ) return false; // already defined
    program.collating_sequence = name;
    return true;
  }
  const char *
  collating_sequence() const {
    assert(!programs.empty());
    return programs.top().collating_sequence;
  }

  cbl_round_t rounded_mode() const { return programs.top().options.default_round; }
  cbl_round_t rounded_mode( cbl_round_t mode ) {
    return programs.top().options.default_round = mode;
  }

  cbl_call_convention_t
  call_convention() {
    return programs.empty()? cbl_call_cobol_e : programs.top().call_convention;
  }
  cbl_call_convention_t
  call_convention( cbl_call_convention_t convention) {
    if( programs.empty() ) {
      return programs.pending_call_convention(convention);
    }
    auto& prog( programs.top() );
    return prog.call_convention = convention;
  }

  const char *
  locale() {
    return programs.empty()? NULL : programs.top().locale.os_name;
  }
  const char *
  locale( const cbl_name_t name ) {
    if( programs.empty() ) return NULL;
    const prog_descr_t::locale_t& locale = programs.top().locale;
    return 0 == strcmp(name, locale.name)? locale.name : NULL;
  }
  const prog_descr_t::locale_t&
  locale( const cbl_name_t name, const char os_name[] ) {
    if( programs.empty() ) {
      static prog_descr_t::locale_t empty;
      return empty;
    }
    return programs.top().locale = prog_descr_t::locale_t(name, os_name);
  }

  bool new_program ( const YYLTYPE& loc, cbl_label_type_t type,
                     const char name[], const char os_name[],
                     bool common, bool initial )
  {
    size_t  parent = programs.empty()? 0 : programs.top().program_index;
    cbl_label_t label = {};
    label.type = type;
    label.parent = parent;
    label.line = yylineno;
    label.common = common;
    label.initial = initial;
    label.os_name = os_name;
    if( !namcpy(loc, label.name, name) ) { gcc_unreachable(); }

    const cbl_label_t *L;
    if( (L = symbol_program_add(parent, &label)) == NULL ) return false;
    programs.push( symbol_index(symbol_elem_of(L)));
    programs.apply_pending();

    bool fOK = symbol_at(programs.top().program_index) + 1 == symbols_end();
    assert(fOK);

    if( (L = symbol_program_local(name)) != NULL ) {
      error_msg(loc, "program '%s' already defined on line %d",
               L->name, L->line);
      return false;
    }

    options_paragraph = cbl_options_t();
    first_statement = 0;

    return fOK;
  }

  void program_needs_initial() { programs.pending_initial(); }

  size_t  program_index(void) const {
    assert(!programs.empty());
    return programs.top().program_index;
  }
  size_t  program_declaratives(void) const {
    if( programs.empty() ) return 0;
    return programs.top().declaratives_index;
  }
  const cbl_label_t * program(void) {
    return programs.empty()?
                NULL : cbl_label_of(symbol_at(programs.top().program_index));
  }
  cbl_label_t * section(void) {
    return programs.empty()? NULL : programs.top().section;
  }
  cbl_label_t * paragraph(void) {
    return programs.empty()? NULL : programs.top().paragraph;
  }

  bool is_first_statement( const YYLTYPE& loc )  {
    if( ! in_declaratives && first_statement == 0 ) {
      if( ! symbol_label_section_exists(program_index()) ) {
        if( ! dialect_ibm() ) {
          error_msg(loc,
                    "Per ISO a program with DECLARATIVES must begin with a SECTION, "
                    "requires -dialect ibm");
        }
      }
      first_statement = loc.first_line;
      return true;
    }
    return false;
  }

  /*
   * At the end of each program, ensure there are no uses of an ambiguous
   * procedure (SECTION or PARAGRAPH) name.  At the end of a top-level program,
   * adjust any CALL targets to use the mangled name of the internal (contained
   * or COMMON ) program.  We ensure there are no duplicate program names, per
   * ISO, in new_program.
   */
  std::set<std::string>  end_program() {
    if( enabled_exceptions.size() ) {
      declaratives_evaluate(ec_none_e);
    }

    assert(!programs.empty());

    procref_t *ref = ambiguous_reference(program_index());
    std::set<std::string> externals = programs.top().external_targets();

    /*
     * For each called local program, replace the original undecorated
     * target with the mangled name.
     *
     * At END-PROGRAM for the top-level program, we know all
     * subprograms, and whether or not they are COMMON. PROGRAM may be
     * the caller, or a subprogram could call COMMON sibling.
     */
    if( programs.size() == 1 ) {
      if( yydebug ) parser_call_targets_dump();
      for( size_t caller : symbol_program_programs() ) {
        const char *caller_name = cbl_label_of(symbol_at(caller))->name;
        for( auto callable : symbol_program_callables(caller) ) {
          auto called = cbl_label_of(symbol_at(callable));
          auto mangled_name =
            called->mangled_name? called->mangled_name : called->name;

          size_t n =
            parser_call_target_update(caller, called->name, mangled_name);
          // Zero is not an error
          dbgmsg("updated %zu calls from #%-3zu (%s) s/%s/%s/",
                 n, caller, caller_name, called->name, mangled_name);
        }
      }
      if( yydebug ) parser_call_targets_dump();
    }

    parser_leave_paragraph( programs.top().paragraph );
    parser_leave_section( programs.top().section );
    programs.pop();

    debugging_clients.clear();
    error_clients.clear();
    exception_clients.clear();

    if( ref ) {
      yywarn("could not resolve paragraph (or section) '%s' at line %d",
               ref->paragraph(), ref->line_number());
      // add string to indicate ambiguity error
      externals.insert(":ambiguous:");
    }
    return externals;
  }

  size_t program_level() const { return programs.size(); }

  size_t program_section() const {
    if( programs.empty() || programs.top().section == NULL ) return 0;
    auto section = programs.top().section;
    return symbol_index(symbol_elem_of(section));
  }

  cbl_label_t *doing_declaratives( bool begin ) {
    if( begin ) {
      in_declaratives = true;
      return NULL;
    }
    assert( !begin );
    in_declaratives = false;
    if( declaratives.empty() ) return NULL;
    assert(!declaratives.empty());

    size_t idcl = symbol_declaratives_add(program_index(), declaratives.as_list());
    programs.top().declaratives_index = idcl;

    // Create section to evaluate declaratives.  Given them unique names so
    // that we can figure out what is going on in a trace or looking at the
    // assembly language.
    static int eval_count=1;
    char eval[32];
    char lave[32];
    sprintf(eval, "_DECLARATIVES_EVAL%d", eval_count);
    sprintf(lave, "_DECLARATIVES_LAVE%d", eval_count);
    eval_count +=1 ;

    struct cbl_label_t*& eval_label = programs.top().declaratives_eval;
    eval_label = label_add(LblSection, eval, yylineno);
    struct cbl_label_t * lave_label = label_add(LblSection, lave, yylineno);
    ast_enter_section(eval_label);
    declarative_runtime_match(cbl_field_of(symbol_at(idcl)), lave_label);
    return lave_label;
  }

  cbl_label_t * new_section( cbl_label_t * section ) {
    std::swap( programs.top().section, section );
    return section;
  }

  /*
   * END DECLARATIVES causes:
   *   1. Add DECLARATIVES symbol, containing criteria blob.
   *   2. Create section _DECLARATIVES_EVAL
   *      and exit label _DECLARATIVES_LAVE
   *   3. declarative_runtime_match generates runtime evaluation "ladder".
   *   4. After a declarative is executed, control branches to the exit label.
   *
   * After each verb, we call declaratives_evaluate,
   * which PERFORMs _DECLARATIVES_EVAL.
   *
   * If the matched declarative is defined by a superior program as
   * GLOBAL, it cannot be PERFORMed.  Instead, it is CALLed with an
   * alternative entry point (TODO).
   */
  void
  declaratives_evaluate( cbl_file_t *file,
                         file_status_t status = FsSuccess ) {
    // The exception file number is assumed to be zero at all times unless
    // it has been set to non-zero, at which point whoever picks it up and takes
    // action on it is charged with setting it back to zero.
    if( file )
      {
      parser_set_file_number((int)symbol_index(symbol_elem_of(file)));
      }
    // parser_set_file_number(file ? (int)symbol_index(symbol_elem_of(file)) : 0);
    parser_set_handled((ec_type_t)status);

    parser_file_stash(file);

    cbl_label_t *eval = programs.first_declarative();
    if( eval ) {
      auto iprog = symbol_elem_of(eval)->program;
      if( iprog  == current_program_index() ) {
        parser_perform(eval);
      } else {
        parser_entry_activate( iprog, eval );
        auto name = cbl_label_of(symbol_at(iprog))->name;
        parser_call( new_literal(strlen(name), name, quoted_e),
                     cbl_refer_t(), 0, NULL, NULL, NULL, false );
      }
    }
  }

  void
  declaratives_evaluate( std::list<cbl_file_t*>& files ) {
    for( auto& file : files ) {
      declaratives_evaluate(file);
    }
  }

  /*
   * To indicate to the runtime-match function that we want to evaluate
   * only the exception condition, unrelated to a file, we set the
   * file register to 0 and the handled-exception register to the
   * handled exception condition (not file status).
   *
   * declaratives_execute performs the "declarative ladder" produced
   * by declaratives_runtime_match.  That section CALLs the
   * runtime-match procedure __gg__match_exception, passing it the
   * values of those two registers.  When that function sees there's
   * no file involved, it interprets the "handled" parameter as
   * ec_type_t, and returns the matching declarative symbol-table
   * index, per usual.
   */
  void
  declaratives_evaluate( ec_type_t handled = ec_none_e ) {
    // The exception file number  is assumed to be zero unless it has been
    // changed to a non-zero value.  The program picking it up and referencing
    // it is charged with setting it back to zero.
    // parser_set_file_number(0);

    parser_set_handled(handled);

    cbl_label_t *eval = programs.first_declarative();
    declarative_execute(eval);
  }

  cbl_label_t * new_paragraph( cbl_label_t *para ) {
    auto& prog( programs.top() );
    auto old(prog.paragraph);
    prog.paragraph = para;
    return old;
  }

  void antecedent_dump() const {
    if( ! yydebug ) return;
    if( ! antecedent_cache.operand ) {
      yywarn( "Antecedent: none" );
    } else {
      yywarn( "Antecedent: %c %s %s %c",
             antecedent_cache.invert? '!':' ',
             name_of(antecedent_cache.operand->field),
             relop_str(antecedent_cache.relop),
             antecedent_cache.has_relop? 'T' : 'F' );
    }
  }
  void antecedent( const rel_part_t& ante ) { antecedent_cache = ante; antecedent_dump(); }
  void antecedent_reset() { antecedent_cache = rel_part_t(); antecedent_dump(); }
  rel_part_t&  antecedent() { return antecedent_cache; }
  rel_part_t&  antecedent( relop_t op ) {
    antecedent_cache.relop_set(op);
    antecedent_dump();
    return antecedent_cache;
  }
  rel_part_t&  antecedent_invert( bool invert=true ) {
    antecedent_cache.invert = invert;
    antecedent_dump();
    return antecedent_cache;
  }

  void compute_begin() { error_labels.generate(); }
  bool in_compute() { return error_labels.on_error != NULL; }
  void compute_end() { error_labels.clear(); }
  cbl_label_t * compute_on_error()  { return error_labels.on_error; }
  cbl_label_t * compute_not_error() { return error_labels.not_error; }
  cbl_label_t * compute_label() { return error_labels.compute_error; }
} current;

#define PROGRAM current.program_index()

static void
add_debugging_declarative( const cbl_label_t * label ) {
  const char *section = current.declarative_section_name();
  if( section ) {
    debugging_clients[label->name].push_back(section);
  }
};

cbl_options_t current_options() {
  return current.options_paragraph;
}

size_t current_program_index() {
  return current.program()? current.program_index() : 0;
}

cbl_label_t * current_section() {
  return current.section();
}
cbl_label_t * current_paragraph() {
  return current.paragraph();
}

const char *
current_declarative_section_name() {
  return current.declarative_section_name();
}

void
add_cobol_exception( ec_type_t type, bool enabled ) {
  current.exception_add( type, enabled );
}

static cbl_round_t rounded_of( int token );

cbl_round_t
current_rounded_mode() {
  return current.rounded_mode();
}

#if needed
static cbl_round_t
current_rounded_mode( cbl_round_t rounded) {
  return current.rounded_mode(rounded);
}
#endif
static cbl_round_t current_rounded_mode( int token );

cbl_call_convention_t
current_call_convention() {
  return current.call_convention();
}
cbl_call_convention_t
current_call_convention( cbl_call_convention_t convention) {
  return current.call_convention(convention);
}

size_t program_level() { return current.program_level(); }

static size_t constant_index( int token );

static relop_t relop_of(int);
static relop_t relop_invert(relop_t op);

static enum classify_t classify_of( int token );

static void subscript_dimension_error( YYLTYPE loc, size_t, const cbl_refer_t *name );

/*
 * Utility functions
 */

char *
normalize_picture( char picture[] );

static inline cbl_field_t *
new_tempnumeric(void) { return new_temporary(FldNumericBin5); }

static inline cbl_field_t *
new_tempnumeric_float(void) { return new_temporary(FldFloat); }

uint32_t
type_capacity( enum cbl_field_type_t type, uint32_t digits );

bool
valid_picture( enum cbl_field_type_t type, const char picture[] );

bool
move_corresponding( cbl_refer_t& tgt, cbl_refer_t& src );

static bool
literal_subscripts_valid( YYLTYPE loc, const cbl_refer_t& name );
static bool
literal_refmod_valid( YYLTYPE loc, const cbl_refer_t& r );

static bool
is_integer_literal( const cbl_field_t *field ) {
  if( is_literal(field) ) {
    int v, n;
    const char *initial = field->data.initial;

    return 1 == sscanf(initial, "%d%n", &v, &n) && n == (int)strlen(initial);
  }
  return false;
}

static inline bool
is_string_literal( const cbl_field_t *field ) {
  return is_literal(field) && is_quoted(field);
}

static inline bool
needs_picture( cbl_field_type_t type ) {
  switch(type) {
  case FldDisplay:
  case FldInvalid:
    gcc_unreachable();
    return false; // not a valid question

  case FldAlphaEdited:
  case FldAlphanumeric:
  case FldNumericBinary:
  case FldNumericDisplay:
  case FldNumericEdited:
  case FldPacked:
    return true;

  case FldFloat:
  case FldNumericBin5:
    return false;

  case FldBlob:
  case FldClass:
  case FldConditional:
  case FldForward:
  case FldGroup:
  case FldIndex:
  case FldLiteralA:
  case FldLiteralN:
  case FldPointer:
  case FldSwitch:
    return false;
  }

  dbgmsg("%s:%d: unknown cbl_field_type_t %u", __func__, __LINE__, type);
  gcc_unreachable();
  return false;
}

static bool
is_callable( const cbl_field_t *field ) {
  switch ( field->type ) {
  case FldInvalid:
  case FldNumericEdited:
  case FldAlphaEdited:
  case FldClass:
  case FldConditional:
  case FldForward:
  case FldSwitch:
  case FldDisplay:
  case FldBlob:
  case FldNumericDisplay:
  case FldNumericBinary:
  case FldFloat:
  case FldPacked:
  case FldNumericBin5:
  case FldLiteralN:
  case FldIndex:
    return false;
  case FldGroup:
  case FldLiteralA:
  case FldAlphanumeric:
  case FldPointer:
    return true;
  }
  cbl_internal_error( "%s:%d: invalid symbol_type_t %d", __func__, __LINE__, field->type );
  return false;
}

/*
 * intrinsic calls
 */
struct cbl_fieldloc_t {
  YYLTYPE loc;
  cbl_field_t *field;

  cbl_fieldloc_t() : loc{ 1,1, 1,1 }, field(NULL) {}
  cbl_fieldloc_t( const YYLTYPE& loc, cbl_field_t *field )
    : loc(loc), field(field)
  {}
};

static size_t
intrinsic_invalid_parameter( int token, const std::vector<cbl_refer_t>& args );

static const char *
intrinsic_cname( int token );

static bool
intrinsic_call_0( cbl_field_t *output, int token ) {
  const char *name = intrinsic_cname(token);
  if( !name ) return false;
  parser_intrinsic_call_0( output, name );
  return true;
}

static bool
intrinsic_call_1( cbl_field_t *output, int token,
                  cbl_refer_t *r1, const YYLTYPE& loc ) {
  std::vector<cbl_refer_t> args { *r1 };
  if( 0 == intrinsic_invalid_parameter(token, args) ) {
    error_msg(loc, "invalid parameter '%s'", r1->field->name);
    return false;
  }

  const char *func = intrinsic_cname(token);
  if( !func ) return false;
  parser_intrinsic_call_1( output, func, *r1 );
  return true;
}

static bool
intrinsic_call_2( cbl_field_t *tgt, int token, cbl_refer_t *r1, cbl_refer_t *r2 ) {
  std::vector<cbl_refer_t> args { *r1, *r2 };
  size_t n = intrinsic_invalid_parameter(token, args);
  if( n < args.size() ) {
    error_msg(args[n].loc, "invalid parameter '%s'", args[n].field->name);
    return false;
  }
  const char *fund = intrinsic_cname(token);
  if( !fund ) return false;
  parser_intrinsic_call_2( tgt, fund, args[0], args[1] );
  return true;
}

static bool
intrinsic_call_3( cbl_field_t *tgt, int token,
                  cbl_refer_t *r1, cbl_refer_t *r2, cbl_refer_t *r3 ) {
  std::vector<cbl_refer_t> args { *r1, *r2, *r3 };
  size_t n = intrinsic_invalid_parameter(token, args);
  if( n < args.size() ) {
    error_msg(args[n].loc, "invalid parameter '%s'", args[n].field->name);
    return false;
  }
  const char *func = intrinsic_cname(token);
  if( !func ) return false;
  parser_intrinsic_call_3( tgt, func, *r1, *r2, *r3 );
  return true;
}

static bool
intrinsic_call_4( cbl_field_t *tgt, int token,
                  cbl_refer_t *r1, cbl_refer_t *r2,
                  cbl_refer_t *r3, cbl_refer_t *r4 ) {
  std::vector<cbl_refer_t> args { *r1, *r2, *r3, *r4 };
  size_t n = intrinsic_invalid_parameter(token, args);
  if( n < args.size() ) {
    error_msg(args[n].loc, "invalid parameter '%s'", args[n].field->name);
    return false;
  }
  const char *func = intrinsic_cname(token);
  if( !func ) return false;
  parser_intrinsic_call_4( tgt, func, *r1, *r2, *r3, *r4 );
  return true;
}

/*
 * Local functions
 */

static inline cbl_field_t *
new_literal( const char initial[] ) {
  return new_literal( strlen(initial), initial );
}

cbl_refer_t *
negate( cbl_refer_t * refer, bool neg = true ) {
  if( ! neg ) return refer;
  assert( is_numeric(refer->field) );
  auto output = new_reference(new_tempnumeric());
  parser_subtract( *output, literally_zero, *refer, current_rounded_mode() );
  return output;
}

cbl_field_t *
conditional_set( cbl_field_t *tgt, bool tf ) {
  static cbl_field_t *one = new_literal("1");

  enum relop_t op = tf? eq_op : ne_op;
  parser_relop( tgt, one, op, one );
  return tgt;
}

static inline cbl_field_t *
table_primary_index( cbl_field_t *table ) {
  assert(table);
  return 0 == table->occurs.indexes.nfield?
    NULL : cbl_field_of(symbol_at(table->occurs.indexes.fields[0]));
}

static inline const cbl_refer_t  // & // Removed the '&' to stop a weird compiler error
invalid_key( const cbl_refer_t& ref ) {
  assert(ref.field);

  if( ref.nsubscript == 0 ) return ref;

  for( size_t i=0; i < ref.nsubscript; i++ ) {
    if( ref.subscripts[i].field->parent != ref.field->parent ) {
      return ref.subscripts[i];
    }
  }
  return NULL;
}

static inline symbol_elem_t *
symbol_find( const std::list<const char *>& names ) {
  auto found = symbol_find(PROGRAM, names);
  if( found.first && !found.second ) {
    auto field = cbl_field_of(found.first);
    yyerror( "%s is not unique, first defined on line %d",
            field->name, field->line );
    return NULL;
  }
  return found.first;
}

static inline cbl_field_t *
field_find( const std::list<const char *>& names ) {
  if( names.size() == 1 ) {
    auto value = cdf_value(names.front());
    if( value ) {
      cbl_field_t * field;
      if( value->is_numeric() ) {
        field = new_tempnumeric();
        parser_set_numeric(field, value->as_number());
      } else {
        field = new_literal(value->string);
      }
      return field;
    }
  }
  symbol_elem_t *e = symbol_find(names);
  return e? cbl_field_of(e) : NULL;
}

static inline symbol_elem_t *
symbol_find( const YYLTYPE& loc, const char *name ) {
  cbl_namelist_t names;
  if( ! name_queue.empty() ) {
    auto names = name_queue.pop_as_names();
  }
  names.push_front(name);
  auto found = symbol_find( PROGRAM, names );
  if( found.first && !found.second ) {
    auto field = cbl_field_of(found.first);
    error_msg(loc, "'%s' is not unique, first defined on line %d",
            field->name, field->line);
    return NULL;
  }
  return found.first;
}

static inline cbl_field_t *
register_find( const char *name ) {
  return cbl_field_of(symbol_register(name));
}

static bool
valid_redefine( const YYLTYPE& loc,
                const cbl_field_t *field, const cbl_field_t *orig ) {
  // Must have same level.
  if( field->level != orig->level ) {
    error_msg(loc, "cannot redefine %s %s as %s %s "
                   "because they have different levels",
              orig->level_str(), orig->name,
              field->level_str(), field->name);
    return false;
  }

  // no higher level intervenes
  /*
   * No entry having a level-number numerically lower than the
   * level-number of data-name-2 may occur between the data
   * description entries of data-name-2 and the subject of the entry.
   */
  struct { symbol_elem_t *field, *orig; } sym = {
    symbol_at(field_index(field)),
    symbol_at(field_index(orig)) };

  auto e = std::find_if( sym.orig + 1, sym.field,
                         [lowest = field->level]( auto& elem ) {
                           if( elem.type != SymField ) return false;
                           auto f = cbl_field_of(&elem);
                           return 0 < f->level && f->level < lowest;
                         } );
  if( e != sym.field ) {
    auto wrong = cbl_field_of(e);
    error_msg(loc, "%s %s on line %d lies between %s and %s",
            wrong->level_str(), wrong->name, wrong->line,
            orig->name, field->name);
    return false;
  }

  // cannot redefine a table
  if( orig->occurs.ntimes() ) {
    error_msg(loc, "cannot redefine table %s %s",
            orig->level_str(), orig->name);
    return false;
  }

  // redefined field cannot be ODO
  if( orig->occurs.depending_on ) {
    error_msg(loc, "redefined data item %s %s has OCCURS DEPENDING ON",
            orig->level_str(), orig->name);
    return false;
  }
  // redefiner cannot have ODO
  if( field->occurs.depending_on ) {
    error_msg(loc, "data item %s %s cannot use REDEFINES and OCCURS DEPENDING ON",
            field->level_str(), field->name);
    return false;
  }

  if( is_variable_length(orig) ) {
    error_msg(loc, "redefined data item %s %s has OCCURS DEPENDING ON",
            orig->level_str(), orig->name);
    return false;
  }
  // We don't know about the redefining group until it's completely defined.

  /*
   * 8) The storage area required for the subject of the entry
   * shall not be larger than the storage area required for the
   * data item referenced by data-name-2, unless the data item
   * referenced by data- name-2 has been specified with level
   * number 1 and without the EXTERNAL clause.
   */
  if( field->type != FldGroup && orig->type != FldGroup ) {
    if( orig->size() < field->size() ) {
      if( orig->level > 1 || orig->has_attr(external_e) ) {
        dbgmsg( "size error orig:  %s", field_str(orig) );
        dbgmsg( "size error redef: %s", field_str(field) );
        error_msg(loc, "%s (%s size %u) larger than REDEFINES %s (%s size %u)",
                  field->name,
                  3 + cbl_field_type_str(field->type), field->size(),
                  orig->name,
                  3 + cbl_field_type_str(orig->type), orig->size() );
      }
    }
  }

  /*
   * 4) No entry having a level-number numerically lower than the
   * level-number of data-name-2 may occur between the data
   * description entries of data-name-2 and the subject of the entry.
   */
  bool same_group = std::none_of( symbol_at(field_index(orig)),
                                  symbol_at(field_index(field)),
                                  [level = field->level]( const auto& elem ) {
                                    if( elem.type == SymField ) {
                                      auto f = cbl_field_of(&elem);
                                      return 0 < f->level && f->level < level;
                                    }
                                    return false;
                                  } );
  if( ! same_group ) {
    error_msg(loc, "cannot redefine %s %s as %s %s "
             "because they belong to different groups",
            orig->level_str(), orig->name,
            field->level_str(), field->name);
    return false;
  }

  return true;
}

static void
field_value_all(struct cbl_field_t * field ) {
  // Expand initial by repeating its contents until it is of length capacity:
  assert(field->data.initial != NULL);
  size_t initial_length = strlen(field->data.initial);
  char *new_initial = static_cast<char*>(xmalloc(field->data.capacity + 1));
  size_t i = 0;
  while(i < field->data.capacity) {
    new_initial[i] = field->data.initial[i%initial_length];
    i += 1;
  }
  new_initial[field->data.capacity] = '\0';
  free(const_cast<char *>(field->data.initial));
  field->data.initial = new_initial;
}

static cbl_field_t *
parent_has_value( cbl_field_t *field ) {
  while( (field = parent_of(field)) != NULL ) {
    if( field->data.initial ) break;
  }
  return field;
}

static uint32_t
group_attr( const cbl_field_t * field ) {
  if( field->parent == 0 ) return 0;

  const symbol_elem_t *e = symbol_at(field->parent);
  if( SymField != e->type ) return 0;

  const cbl_field_t *p = cbl_field_of(e);
  if( p->type != FldGroup ) return 0;

  return p->attr;
}

static struct symbol_elem_t *
field_of( const char F[], int L, const char name[] ) {
  struct symbol_elem_t *e = symbol_field(PROGRAM, 0, name);
  if( !e ) {
    cbl_internal_error("%s:%d: no symbol '%s' found", F, L, name);
  }
  assert( procedure_div_e != current_division  );
  return e;
}
#define field_of( F ) field_of(__func__, __LINE__, (F))

static struct cbl_field_t *
field_add( const YYLTYPE& loc, cbl_field_t *field ) {
  switch(current_data_section) {
  case not_data_datasect_e:
  case file_datasect_e:
  case working_storage_datasect_e:
    break;
  case local_storage_datasect_e:
    field->attr |= local_e;
    break;
  case linkage_datasect_e:
    field->attr |= linkage_e;
    break;
  }

  // Use isym 0 to indicate the location of the field under construction.
  symbol_field_location(0, loc);

  struct symbol_elem_t *e = symbol_field_add(PROGRAM, field);
  if( !e ) return NULL;
  symbol_field_location(symbol_index(e), loc);
  field = cbl_field_of(e);
  assert(field->type != FldDisplay);

  if( field->parent == 0 ) {
    switch(field->level) {
    case 0: case 1: case 77: case 78:
      break;
    default:
      error_msg(loc, "%s %s is not part of an 01 record",
              field->level_str(), field->name );
      return NULL;
      break;
    }
  }
  return field;
}

static const char *
field_attr_str( const cbl_field_t *field ) {
  static const std::vector<cbl_field_attr_t> attrs {
    figconst_1_e, figconst_2_e, figconst_4_e, rjust_e, ljust_e,
    zeros_e, signable_e, constant_e, function_e, quoted_e, filler_e,
    intermediate_e, embiggened_e, all_alpha_e, all_x_e,
    all_ax_e, prog_ptr_e, scaled_e, refmod_e, based_e, any_length_e,
    global_e, external_e, blank_zero_e, linkage_e, local_e, leading_e,
    separate_e, envar_e, dnu_1_e, bool_encoded_e, hex_encoded_e,
    depends_on_e, initialized_e, has_value_e, ieeedec_e, big_endian_e,
    same_as_e, record_key_e, typedef_e, strongdef_e,
  };
  return field->attr_str(attrs);
}

static bool
uniform_picture( const char *picture, char model ) {
  const char *eopicture( picture + strlen(picture) );
  model = TOLOWER(model);
  return std::all_of(picture, eopicture,
                     [model]( char ch ) {
                       return model == TOLOWER(ch);
                     } );
}

static enum cbl_field_attr_t
uniform_picture( const char *picture ) {
  static char ch[] = { 'A', 'X' };
  for( auto p = ch; p < ch + sizeof(ch); p++ ) {
    if( uniform_picture(picture, *p) ) {
      switch(*p) {
      case 'A': return all_alpha_e;
      case 'X': return all_x_e;
      }
    }
  }
  return none_e;
}

static bool
field_type_update( cbl_field_t *field, cbl_field_type_t type,
                   YYLTYPE loc,
                   bool is_usage = false)
{
  // preserve NumericEdited if already established
  if( !is_usage && field->has_attr(blank_zero_e) ) {
    if( type == FldNumericDisplay && field->type == FldNumericEdited ) {
      return true;
    }
  }

  // disallow USAGE if inherited from parent (all members must be of same type)
  if( is_usage && field->usage != type ) {
    switch( field->usage ) {
    case FldInvalid:
    case FldDisplay:
      break; // ok
    default:
      error_msg(loc, "cannot set %s to USAGE %s "
               "because the group is restricted to USAGE %s",
               field->name, cbl_field_type_str(type),
               cbl_field_type_str(field->usage));
      return false;
    }
  }

  if( ! symbol_field_type_update(field, type, is_usage) ) {
    error_msg(loc, "cannot set USAGE of %s to %s (from %s)", field->name,
             cbl_field_type_str(type) + 3, cbl_field_type_str(field->type) + 3);
    return false;
  }

  dbgmsg( "%s:%d: %s became %s based on %s", __func__, __LINE__, field->name,
          cbl_field_type_str(field->type), cbl_field_type_str(type) );

  return true;
}

static bool
field_capacity_error( const YYLTYPE& loc, const cbl_field_t *field ) {
  uint32_t parent_capacity = 0;
  if( field->parent ) {
    auto e = symbol_at(field->parent);
    if( e->type == SymField ) parent_capacity = cbl_field_of(e)->data.capacity;
  }
  /*
   * Field may become a table whose capacity was inherited from a parent with
   * data. If so, the field's capacity will be overwritten by its
   * PICTURE-defined size.
   */
  if( parent_capacity < field->data.capacity && !symbol_redefines(field) ) {
    dbgmsg( "%s: %s", __func__, field_str(field) );
    error_msg(loc,  "%s has USAGE incompatible with PICTURE",
              field->name );
    return true;
  }
  return false;
}
#define ERROR_IF_CAPACITY(L, F)                                 \
  do { if( field_capacity_error(L, F) ) YYERROR; } while(0)

static const char *
blank_pad_initial( const char initial[], size_t capacity, size_t new_size ) {
  assert(capacity < new_size);
  assert(initial != NULL);

  if( normal_value_e != cbl_figconst_of(initial) ) return initial;

  auto p = reinterpret_cast<char *>( xmalloc(2 + new_size) );
  memset(p, 0x20, new_size);
  memcpy(p, initial, capacity);
  p[new_size] = '\0'; // for debugging
  p[++new_size] = '\0'; // for debugging
  return p;
}

static bool
value_encoding_check( const YYLTYPE& loc, cbl_field_t *field ) {
  if( ! field->internalize() ) {
    error_msg(loc, "inconsistent string literal encoding for '%s'",
              field->data.initial);
    return false;
  }
  return true;
}


#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"

static struct cbl_field_t *
field_alloc( const YYLTYPE& loc, cbl_field_type_t type, size_t parent, const char name[] ) {
  cbl_field_t *f, field = {};
  field.type = type;
  field.usage = FldInvalid;
  field.parent = parent;
  field.line = yylineno;
  
  if( !namcpy(loc, field.name, name) ) return NULL;
  f = field_add(loc, &field);
  assert(f);
  return f;
}

static const cbl_file_t protofile;

// Add a file to the symbol table with its record area field.
// The default organization is sequential.
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat-truncation"
static cbl_file_t *
file_add( YYLTYPE loc, cbl_file_t *file ) {
  gcc_assert(file);
  enum { level = 1 };
  struct cbl_field_t area = { 0, FldAlphanumeric, FldInvalid, 0, 0,0, level, {}, yylineno },
                     *field = field_add(loc, &area);
  file->default_record = field_index(field);

  // install file, and set record area's name
  auto e = symbol_file_add(PROGRAM, file);
  if( !e ) {
    error_msg(loc, "%s was defined previously on line %d", file->name, file->line);
    return NULL;
  }
  file = cbl_file_of(e);
  snprintf(field->name, sizeof(field->name),
           "%s%zu_%s",
           record_area_name_stem, symbol_index(e), file->name);
  if( file->attr & external_e ) {
    snprintf(field->name, sizeof(field->name),
             "%s%s", record_area_name_stem, file->name);
  }
  field->file = field->parent = symbol_index(e);

  return file;
}
#pragma GCC diagnostic pop
#pragma GCC diagnostic pop


static cbl_alphabet_t *
alphabet_add( const YYLTYPE& loc, cbl_encoding_t encoding ) {
  cbl_alphabet_t alphabet(loc, encoding);
  symbol_elem_t *e = symbol_alphabet_add(PROGRAM, &alphabet);
  assert(e);
  return cbl_alphabet_of(e);
}

// The current field always exists in the symbol table, even if it's incomplete.
static cbl_field_t *
current_field(cbl_field_t * field = NULL) {
  static cbl_field_t *local;
  if( field ) local = field;
  gcc_assert(field_index(local));
  return local;
}

static struct cbl_special_name_t *
special_of( const char F[], int L, const char name[] ) {
  struct symbol_elem_t *e = symbol_special(PROGRAM, name);
  if( !e ) {
    dbgmsg("%s:%d: no special symbol '%s' found", F, L, name);
    return NULL;
  }
  return cbl_special_name_of(e);
}
#define special_of( F ) special_of(__func__, __LINE__, (F))

static inline void
parser_add2( struct cbl_num_result_t& to,
             struct cbl_refer_t from ) {
  parser_add(to.refer, to.refer, from, to.rounded);
}

static inline void
parser_subtract2( struct cbl_num_result_t to,
                  struct cbl_refer_t from ) {
  parser_subtract(to.refer, to.refer, from, to.rounded);
}

static bool
parser_move_carefully( const char */*F*/, int /*L*/,
                       tgt_list_t *tgt_list,
                       const cbl_refer_t& src,
                       bool is_index )
{
  for( const auto& num_result : tgt_list->targets ) {
    const cbl_refer_t& tgt = num_result.refer;

    if( is_index ) {
      if( tgt.field->type != FldIndex && src.field->type != FldIndex) {
        error_msg(src.loc, "invalid SET %s (%s) TO %s (%s): not a field index",
                  tgt.field->name, cbl_field_type_str(tgt.field->type),
                  src.field->name, cbl_field_type_str(src.field->type));
        delete tgt_list;
        return false;
      }
    } else {
      if( ! valid_move( tgt.field, src.field ) ) {
        if( ! is_index ) {
          char ach[16];
          char stype[32];
          char dtype[32];
          strcpy(stype, cbl_field_type_str(src.field->type));
          strcpy(dtype, cbl_field_type_str(tgt.field->type));

          if( src.field->attr & all_alpha_e )
            {
            strcpy(stype, "FldAlphabetic");
            }
          if( tgt.field->attr & all_alpha_e )
            {
            strcpy(dtype, "FldAlphabetic");
            }
          if( !(src.field->attr & scaled_e) && src.field->data.rdigits )
            {
            sprintf(ach, ".%d", src.field->data.rdigits);
            strcat(stype, ach);
            }
          if( !(tgt.field->attr & scaled_e) && tgt.field->data.rdigits )
            {
            sprintf(ach, ".%d", tgt.field->data.rdigits);
            strcat(dtype, ach);
            }

          error_msg(src.loc,  "cannot MOVE '%s' (%s) to '%s' (%s)",
                    name_of(src.field), stype,
                    name_of(tgt.field), dtype);
          delete tgt_list;
          return false;
        }
      }
    }
  }
  size_t ntgt = tgt_list->targets.size();
  std::vector <cbl_refer_t>  tgts(ntgt);
  std::transform( tgt_list->targets.begin(), tgt_list->targets.end(), tgts.begin(),
                  []( const cbl_num_result_t& res ) { return res.refer; } );
  parser_move(ntgt, tgts.data(), src);
  delete tgt_list;
  return true;
}
#define parser_move2(P, S) \
        parser_move_carefully(__func__, __LINE__, (P), (S), false)
#define parser_index(P, S) \
        parser_move_carefully(__func__, __LINE__, (P), (S), true)

static void
ast_set_pointers( const list<cbl_num_result_t>& tgts, cbl_refer_t src ) {
  assert(!tgts.empty());
  assert(src.field || src.prog_func);
  size_t nptr = tgts.size();
  std::vector <cbl_refer_t> ptrs(nptr);

  std::transform( tgts.begin(), tgts.end(), ptrs.begin(), cbl_num_result_t::refer_of );
  parser_set_pointers(nptr, ptrs.data(), src);
}

void
stringify( refer_collection_t *inputs,
           cbl_refer_t into, cbl_refer_t pointer,
           cbl_label_t  *on_error = NULL,
           cbl_label_t *not_error = NULL);

void unstringify( cbl_refer_t& src, refer_list_t *delimited,
                  unstring_into_t * into,
                  cbl_label_t  *on_error = NULL,
                  cbl_label_t *not_error = NULL );

static cbl_label_t *
implicit_paragraph()
{
  cbl_name_t name;
  sprintf(name, "_implicit_paragraph_%zu", symbol_index());
  // Programs have to start with an implicit paragraph
  return label_add(LblParagraph, name, yylineno);
}
static cbl_label_t *
implicit_section()
{
  cbl_name_t name;
  sprintf(name, "_implicit_section_%zu", symbol_index());
  // Programs have to start with an implicit section
  return label_add(LblSection, name, yylineno);
}

static void
ast_enter_exit_section( cbl_label_t * section ) {
  auto implicit = section?  implicit_paragraph() : NULL;

  struct { cbl_label_t *para, *sect;
    inline bool exists() const { return sect != NULL && para != NULL; }
  } prior = {
    current.new_paragraph(implicit),
    current.new_section(section)
  };
  if( false && yydebug ) {
    fprintf(stderr, "( %d ) %s:%d: leaving section %s paragraph %s\n",
            yylineno, __func__, __LINE__,
            prior.sect? prior.sect->name : "''",
            prior.para? prior.para->name : "''");
  }
  if( prior.exists() ) {
    parser_leave_paragraph(prior.para);
    parser_leave_section(prior.sect);
  }
  if( section ) {
    parser_enter_section(section);
    parser_enter_paragraph(implicit);
  }
}

static inline void
ast_enter_section( cbl_label_t * section ) {
  assert(section);
  section->lain = yylineno;
  ast_enter_exit_section( section );
}

static inline void
ast_exit_section() {
  ast_enter_exit_section( NULL );
}

static void
ast_enter_paragraph( cbl_label_t * para ) {
  para->lain = yylineno;
  cbl_label_t *prior  = current.new_paragraph(para);
  if( prior ) {
    parser_leave_paragraph(prior);
  }
  parser_enter_paragraph(para);
}

static bool
data_division_ready() {
  // Install and use any alphabets.
  if( nparse_error == 0 ) { // error might have stemmed from the alphabet itself
    const char *name = current.collating_sequence();

    if( ! symbols_alphabet_set(PROGRAM, name) ) {
      error_msg(yylloc, "no alphabet '%s' defined", name);
      return false;
    }
  }

  // Tell codegen about symbols.
  static size_t nsymbol = 0;
  if( (nsymbol = symbols_update(nsymbol, nparse_error == 0)) > 0 ) {
    if( ! literally_one ) {
      literally_one = new_literal("1");
      literally_zero = new_literal("0");
    }
  }

  if( nsymbol == 0 || nparse_error > 0 ) {
    dbgmsg( "%d errors in DATA DIVISION, compilation ceases", nparse_error );
    return false;
  }

  return true;
}

static
bool
anybody_redefines(cbl_field_t *tree)
  {
  bool retval = false;
  while(tree)
    {
    if( symbol_redefines(tree) )
      {
      retval = true;
      break;
      }
    tree = parent_of(tree);
    }
  return retval;
  }

static bool
procedure_division_ready( YYLTYPE loc, cbl_field_t *returning, ffi_args_t *ffi_args ) {
  auto prog = cbl_label_of(symbols_begin(current.program_index()));

  if( prog->type == LblFunction ) {
    if( ! returning ) {
      error_msg(loc, "FUNCTION %s requires RETURNING", prog->name);
      return false;
    } else {
      prog->returning = field_index(returning);
    }
    current.udf_update(ffi_args);
  }

  if( returning ) {
    if( ! (returning->level == 1 || returning->level == 77) ) {
      error_msg(loc, "RETURNING %s must be level 01 or 77", returning->name);
    }
    if( symbol_redefines(returning) ) {
      error_msg(loc, "RETURNING %s cannot REDFINE anything", returning->name);
    }
  }
  if( ffi_args ) {
    size_t i=0;
    for( const auto& arg : ffi_args->elems ) {
      auto field = arg.refer.field;
      i++;
      if( returning == field ) {
        error_msg(loc, "RETURNING %s duplicates USING parameter %zu",
                 returning->name, i);
      }
      if( ! (field->level == 1 || field->level == 77) ) {
        error_msg(loc, "USING %s must be level 01 or 77",
                 field->name);
      }
      if( symbol_redefines(field) ) {
        error_msg(loc, "USING %s cannot REDEFINE anything",
                 field->name );
      }
    }
  }

  // Start the Procedure Division.
  size_t narg = ffi_args? ffi_args->elems.size() : 0;
  std::vector <cbl_ffi_arg_t> args(narg);
  cbl_ffi_arg_t*pargs = NULL;
  if( narg > 0 ) {
    std::copy(ffi_args->elems.begin(), ffi_args->elems.end(), args.begin());
    pargs = args.data();
  }

  // Create program initialization section.  We build it on an island,
  // that gets executed only if the program is IS INITIAL, or when the
  // program is the subject of a CANCEL statement.

  static const char init[] = "_INITIALIZE_PROGRAM";
  static const char tini[] = "_INITIALIZE_DONE";

  struct cbl_label_t * init_label = label_add(LblSection, init, yylineno);
  struct cbl_label_t * tini_label = label_add(LblSection, tini, yylineno);

  // parser_division(procedure_div_e) needs initial_section:
  prog->initial_section = symbol_index(symbol_elem_of(init_label));

  if( current.program_index() > 1 ) {
    ast_exit_section();
  }
  parser_division( procedure_div_e, returning, narg, pargs );

  std::for_each( symbols_begin(current.program_index()), symbols_end(),
                 []( auto& elem ) {
                   if( elem.type == SymField ) {
                     auto f = cbl_field_of(&elem);
                     if( f->has_attr(local_e) ) {
                       parser_local_add(f);
                     }
                   }
                 } );

  // At this point we count up the number of variables that will need to be
  // initialized in _INITIALIZE_PROGRAM:
  int count_of_variables = 0;
  for( symbol_elem_t *e =
         symbols_begin(1 + current.program_index());
       e < symbols_end(); e++ ) {
    if( is_program(*e) ) break;
    if( e->type != SymField ) continue;
    cbl_field_t *f = cbl_field_of(e);
    if( !f->var_decl_node )
      {
      // This can happen when there was an error parsing the data division
      continue;
      }
    if( f->type == FldForward ) continue;
    if( f->type == FldLiteralA ) continue;
    if( anybody_redefines(f) ) continue;
    if( f->has_attr(linkage_e) ) continue;
    if( f->has_attr(local_e) ) continue;
    if( f->is_typedef() ) {
      auto isym = end_of_group( symbol_index(e) );
      e = symbol_at(--isym);
      continue;
    }
    count_of_variables += 1;
  }
  // Allocate space for the static table of variables
  parser_init_list_size(count_of_variables);

  // Do a second pass:
  // Initialize the static table with the variables:
  for( symbol_elem_t *e =
         symbols_begin(1 + current.program_index());
       e < symbols_end(); e++ ) {
    if( is_program(*e) ) break;
    if( e->type != SymField ) continue;
    cbl_field_t *f = cbl_field_of(e);
    if( !f->var_decl_node )
      {
      // This can happen when there was an error parsing the data division
      continue;
      }
    if( f->type == FldForward ) continue;
    if( f->type == FldLiteralA ) continue;
    if( anybody_redefines(f) ) continue;
    if( f->has_attr(linkage_e) ) continue;
    if( f->has_attr(local_e) ) continue;
    if( f->is_typedef() ) {
      auto isym = end_of_group( symbol_index(e) );
      e = symbol_at(--isym);
      continue;
    }
    parser_init_list_element(f);
  }

  // This is where we jump over the island
  parser_label_goto(tini_label);

  // And here we create the initialization section:
  ast_enter_section(init_label);  // _INITIALIZE_PROGRAM section.

  parser_init_list();

  // Lay down an implicit section to end the init_label
  ast_enter_section(implicit_section());

  // This is the end of the island
  parser_label_label(tini_label);

  if( current.program()->initial ) {
    // We perform the section we just layed down when IS INITIAL
    parser_perform(init_label);
  }
  return true;
}

static size_t file_section_fd;
static size_t current_sort_file;

static bool
file_section_fd_set( file_entry_type_t type, char name[], const YYLTYPE& loc ) {
  static std::set<size_t> has_fd;

  // File must have been uniquely created by SELECT.
  // FD names are also unique within a program.
  auto e = symbol_file(PROGRAM, name);
  if( !e ) {
    error_msg(loc, "file name not found");
    return false;
  }

  file_section_fd = symbol_index(e);
  auto result = has_fd.insert(file_section_fd);
  if( !result.second ) {
    auto f = cbl_file_of(e);
    const char *type_str = "???";
    switch(type) {
    case fd_e: type_str = "FD"; break;
    case sd_e: type_str = "SD"; break;
    }
    error_msg(loc, "%s %s previously defined on line %d",
            type_str, f->name, f->line);
    return false;
  }

  auto& file(*cbl_file_of(e));
  file.entry_type = type;

  if( file.org == file_disorganized_e ) {
    file.org = file_sequential_e;
  }

  return file_section_fd > 0;
}

/*
 * While in the File Section, set the parent of each 01 to be the FD
 * default_record, and its file member to the file's symbol index.
 */
static bool
file_section_parent_set( cbl_field_t *field ) {
  if( symbol_at(file_section_fd)->type == SymFile ) {
    auto file = cbl_file_of(symbol_at(file_section_fd));
    auto record_area = cbl_field_of(symbol_at(file->default_record));

    record_area->data.capacity = std::max(record_area->data.capacity,
                                                field->data.capacity);

    field->file = file_section_fd;
    auto redefined = symbol_redefines(record_area);
    field->parent = redefined? record_area->parent : file->default_record;
  }
  return file_section_fd > 0;
}

void ast_call(const YYLTYPE& loc, cbl_refer_t name,
                  cbl_refer_t returning,
                  size_t narg, cbl_ffi_arg_t args[],
                  cbl_label_t *except,
                  cbl_label_t *not_except,
                  bool is_function );

cbl_field_t *
ast_file_status_between( file_status_t lower, file_status_t upper );

void internal_ebcdic_lock();
void internal_ebcdic_unlock();

void
ast_end_program(const char name[]  ) {
  std::for_each( symbols_begin(), symbols_end(),
                 []( const auto& elem ) {
                   if( elem.type == SymLabel ) {
                     auto& L( *cbl_label_of(&elem) );
                     if( L.used )  {
                       if( ! L.lain ) {
                         YYLTYPE loc { L.line, 1, L.line, 1 };
                         error_msg(loc, "line %d: %s "
                                   "is used on line %d and never defined",
                                   L.line, L.name, L.used );
                       }
                       dbgmsg("label: %.20s: %d/%d/%d",
                              L.name, L.line, L.lain, L.used);
                     }
                   }
                 } );
  if( current_program_index() == 0 ) {
    parser_program_hierarchy( cbl_prog_hier_t() );
  } else {
    ast_exit_section();
  }
  parser_end_program(name);
  internal_ebcdic_unlock();
}

static bool
goodnight_gracie() {
  const cbl_label_t *prog = current.program();
  assert(prog);

  std::set<std::string> externals = current.end_program();

  if( !externals.empty() ) {
    for( const auto& name : externals ) {
      yywarn("%s calls external symbol '%s'",
            prog->name, name.c_str());
    }
    return false;
  }

  // pointer still valid because name is in symbol table
  ast_end_program(prog->name);
  return true;
}

const char * keyword_str( int token );

static YYLTYPE current_location;

const YYLTYPE& cobol_location() { return current_location; }

static inline YYLTYPE
location_set( const YYLTYPE& loc ) {
  return current_location = loc;
}

static int prior_statement;

static size_t statement_begin( const YYLTYPE& loc, int token );

static void ast_first_statement( const YYLTYPE& loc ) {
  if( current.is_first_statement( loc ) ) {
    parser_first_statement(loc.first_line);
  }
}

#pragma GCC diagnostic push
