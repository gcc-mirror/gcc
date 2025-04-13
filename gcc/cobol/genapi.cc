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
#include "cobol-system.h"

#include "coretypes.h"
#include "tree.h"
#include "tree-iterator.h"
#include "stringpool.h"
#include "diagnostic-core.h"
#include "target.h"

#include "../../libgcobol/ec.h"
#include "../../libgcobol/common-defs.h"
#include "util.h"
#include "cbldiag.h"
#include "symbols.h"
#include "gengen.h"
#include "inspect.h"
#include "../../libgcobol/io.h"
#include "genapi.h"
#include "genutil.h"
#include "genmath.h"
#include "structs.h"
#include "../../libgcobol/gcobolio.h"
#include "../../libgcobol/charmaps.h"
#include "../../libgcobol/valconv.h"
#include "show_parse.h"
#include "fold-const.h"
#include "realmpfr.h"

extern int yylineno;

#define TSI_BACK (tsi_last(current_function->statement_list_stack.back()))

extern char *cobol_name_mangler(const char *cobol_name);
static tree gg_attribute_bit_get(struct cbl_field_t *var, cbl_field_attr_t bits);

static tree label_list_out_goto;
static tree label_list_out_label;
static tree label_list_back_goto;
static tree label_list_back_label;

static void hijack_for_development(const char *funcname);

static size_t sv_data_name_counter = 1;
static int call_counter = 1;
static int pseudo_label = 1;

static bool suppress_cobol_entry_point = false;
static char ach_cobol_entry_point[256] = "";

bool bSHOW_PARSE = getenv("GCOBOL_SHOW");
bool show_parse_sol = true;
int  show_parse_indent = 0;

#define DEFAULT_LINE_NUMBER 2

#ifdef LINE_TICK
/*  This code is used from time to time when sorting out why compilation
    takes more time than expected */
static void
line_tick()
  {
  using namespace std::chrono;
  static high_resolution_clock::time_point t1 = high_resolution_clock::now();
  static high_resolution_clock::time_point t2;
  int line_now = CURRENT_LINE_NUMBER;
  static int line = 0;
  if( (line_now / 10000) != (line / 10000) )
    {
    line = line_now;
    t2 = high_resolution_clock::now();
    duration<double> time_span = duration_cast<duration<double>>(t2 - t1);
    fprintf(stderr, "%6d %6.1lf\n", line, time_span.count());
    }
  }
#else
#define line_tick()
#endif

typedef struct TREEPLET
  {
  tree pfield;
  tree offset;
  tree length;
  } TREEPLET;

static
void
treeplet_fill_source(TREEPLET &treeplet, cbl_refer_t &refer)
  {
  treeplet.pfield = gg_get_address_of(refer.field->var_decl_node);
  treeplet.offset = refer_offset_source(refer);
  treeplet.length = refer_size_source(refer);
  }

tree file_static_variable(tree type, const char *v)
  {
  // This routine returns a reference to an already-defined file_static variable
  // You need to know the type that was used for the definition.
  return gg_declare_variable(type, v, NULL, vs_file_static);
  }

static void move_helper(tree        size_error,  // INT
                        cbl_refer_t destref,
                        cbl_refer_t sourceref,
                        TREEPLET    &tsource,
                        cbl_round_t rounded,
                        bool check_for_error,
                        bool restore_on_error = false
                        );

// set using -f-trace-debug, defined in lang.opt
int f_trace_debug;

// When doing WRITE statements, the IBM Language Reference and the ISO/IEC_2014
// standard specify that when the ADVANCING clause is omitted, the default is
// AFTER ADVANCING 1 LINE.
//
// MicroFocus and GnuCOBOL state that the default is BEFORE ADVANCING 1 LINE
//
// During initial compiler development, we used Michael Coughlin's "Beginning
// COBOL For Programmers" textbook for source code examples, and it was clear
// from at least one sample program that his compiler used the Microfocus
// convention.  For ease of development, we took on that same convention, but
// we provide here for a switch that changes that behavior:

static bool auto_advance_is_AFTER_advancing = 0;

/*  This is a little complicated.  In order to keep things general, we are
    assuming that any function we call will be returning a 64-bit value.  In
    places where we know that not to be true, we'll have to do appropriate
    casts.  For example, main() returns an INT, as do functions that
    return the default RETURN-CODE will have */

#define COBOL_FUNCTION_RETURN_TYPE SSIZE_T

#define MAX_AFTERS 8

// These variables contol a little state machine.  When a simple -main is in
// effect, the first program in the module becomes the target of a main()
// that we synthesize function.  When -main=module:progid is in effect, we
// create a main() that calls progid.  When active, progid is kept in
// the map main_strings.
static std::unordered_map<std::string, std::string> main_strings;
static bool this_module_has_main = false;   // sticky switch for the module
static bool next_program_is_main = false;   // transient switch for the module
static char *main_entry_point = NULL;

static bool static_call = true;
bool use_static_call( bool yn ) { return static_call = yn; }
static bool use_static_call() { return static_call; }

// This global variable can be set upstream, like from a compiler
// command line switch.  "1" for stdout, "2" for stderr, or "filename"

const char *gv_trace_switch = NULL;

// The environment variable wins over the command line
char const *bTRACE1 = NULL;
tree trace_handle;
tree trace_indent;
bool cursor_at_sol = true;

static void
trace1_init()
  {
  static bool first_time = true;
  if( first_time )
    {
    first_time = false;
    trace_handle = gg_define_variable(INT, "trace_handle", vs_static);
    trace_indent = gg_define_variable(INT, "trace_indent", vs_static);

    bTRACE1 = getenv("GCOBOL_TRACE") ? getenv("GCOBOL_TRACE") : gv_trace_switch;

    if( bTRACE1 && strcmp(bTRACE1, "0") != 0 )
      {
      if( strcmp(bTRACE1, "1") == 0 )
        {
        gg_assign(trace_handle , integer_one_node);
        }
      else if( strcmp(bTRACE1, "2") == 0 )
        {
        gg_assign(trace_handle , integer_two_node);
        }
      else
        {
        gg_assign(trace_handle ,
                  gg_open(gg_string_literal(bTRACE1),
                          build_int_cst_type(INT, O_CREAT|O_WRONLY|O_TRUNC)));
        }
      }
    else
      {
      // In case bTRACE1 pointed to an empty string
      bTRACE1 = NULL;
      }
    }
  }

static void
create_cblc_string_variable(const char *var_name, const char *var_contents)
  {
  // This is a way of having the compiler communicate with GDB.  I create a
  // global const char[] string with a known name so that GDB can look for that
  // variable and pick up its contents.

  // This probably should be in the .debug_info section, but for the moment I
  // don't know how to do that, but I do know how to do this:

  tree array_of_characters = build_array_type_nelts(CHAR, strlen(var_contents)+1);
  TYPE_NAME(array_of_characters) = get_identifier("cblc_string");
  tree constr = build_string(strlen(var_contents)+1, var_contents);
  TREE_TYPE(constr) = array_of_characters;
  TREE_STATIC(constr)    = 1;
  TREE_CONSTANT(constr)  = 1;
  tree entry_point = gg_declare_variable(array_of_characters,
                                         var_name,
                                         constr,
                                         vs_external);
  gg_define_from_declaration(entry_point);
  }

static void
build_main_that_calls_something(const char *something)
  {
  // This routine generates main(), which has as its body a call to "something".
  // which is a call to a simple `extern int something(void)` routine.

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" main will call ")
    SHOW_PARSE_TEXT(something)
    SHOW_PARSE_END
    }

  gg_set_current_line_number(DEFAULT_LINE_NUMBER);

  gg_define_function( INT,
                      "main",
                      INT, "argc",
                      build_pointer_type(CHAR_P), "argv",
                      NULL_TREE);

  // Pick up pointers to the input parameters:
  // First is the INT which is the number of argv[] entries
  tree argc         = DECL_ARGUMENTS(current_function->function_decl);
  // Second is the char **argv
  tree argv         = TREE_CHAIN(argc);          // overall source length

  gg_call(  VOID,
            "__gg__stash_argc_argv",
            argc,
            argv,
            NULL_TREE);

  // Call the top-level COBOL function.  We know it has to return an INT,
  // so we need to cast it from the SIZE_T that all COBOL are assumed
  // to return:

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT_ABC("main calls \"", something, "\"")
    TRACE1_END
    }

  // Let MODULE-NAME know that we were launched by a generated -main program
  gg_call(VOID,
          "__gg__module_name_push",
          gg_string_literal("Mmain"),
          NULL_TREE);

  char *psz = cobol_name_mangler(something);
  gg_assign(var_decl_main_called, integer_one_node);
  gg_return(gg_cast(INT, gg_call_expr( COBOL_FUNCTION_RETURN_TYPE,
                                       psz,
                                       argc,
                                       argv,
                                       NULL_TREE)));
  strncpy(ach_cobol_entry_point, psz, sizeof(ach_cobol_entry_point)-1);
  free(psz);
  gg_finalize_function();
  }

static std::unordered_map<std::string, size_t>gotos_labels;
#define LABEL_COUNT_OFFSET 100

static
tree
get_field_p(size_t index)
  {
  if(index)
    {
    cbl_field_t *field = cbl_field_of(symbol_at(index));

    if( !field->var_decl_node )
      {
      dbgmsg("%s (type: %s) improperly has a NULL var_decl_node",
                  field->name,
                  cbl_field_type_str(field->type));
      cbl_internal_error(
                "Probable cause: it was referenced without being defined.");
      }

    return gg_get_address_of(field->var_decl_node);
    }
  else
    {
    return gg_cast(cblc_field_p_type_node, null_pointer_node);
    }
  }

static
char *
level_88_helper(size_t parent_capacity,
                const cbl_domain_elem_t &elem,
                size_t &returned_size)
  {
  // We return a MALLOCed return value, which the caller must free.
  char *retval  = (char *)xmalloc(parent_capacity + 64);
  char *builder = (char *)xmalloc(parent_capacity + 64);
  size_t nbuild = 0;

  cbl_figconst_t figconst = cbl_figconst_of( elem.name());
  if( figconst )
    {
    nbuild = 1;
    strcpy(retval, "1Fx");
    switch(figconst)
      {
      case normal_value_e :
        // This really should never happend
        abort();
        break;
      case low_value_e    :
        retval[2] = 'L';
        break;
      case zero_value_e   :
        retval[2] = 'Z';
        break;
      case space_value_e  :
        retval[2] = 'S';
        break;
      case quote_value_e  :
        retval[2] = 'Q';
        break;
      case high_value_e   :
        retval[2] = 'H';
        break;
      case null_value_e:
        retval[2] = '\0';
        break;
      }
    returned_size = 3;
    }
  else
    {
    // We are working with an ordinary string.

    // Pick up the string
    size_t first_name_length  = elem.size();
    char *first_name = (char *)xmalloc(first_name_length + 1);
    memcpy(first_name, elem.name(), first_name_length);
    first_name[first_name_length] = '\0';

    // Convert it to EBCDIC, when necessary; leave it alone when not necessary.
    for(size_t i=0; i<first_name_length; i++)
      {
      first_name[i] = ascii_to_internal(first_name[i]);
      }

    if( parent_capacity == 0 )
      {
      // Special case:  parent_capacity is zero when this routine has been
      // called as part of a debugging trace.
      if( elem.all )
        {
        strcpy(builder+nbuild, "ALL ");
        nbuild += 4;
        }
      memcpy(builder+nbuild, first_name, first_name_length);
      nbuild += first_name_length;
      }
    else
      {
      if( elem.all )
        {
        while(nbuild < parent_capacity )
          {
          builder[nbuild] = first_name[nbuild % first_name_length];
          nbuild += 1;
          }
        }
      else
        {
        memcpy(builder+nbuild, first_name, first_name_length);
        nbuild += first_name_length;
        }
      }
    returned_size = sprintf(retval, "%zdA", nbuild);
    memcpy(retval + returned_size, builder, nbuild);
    returned_size += nbuild;
    free(first_name);
    free(builder);
    }
  return retval;
  }

static char *
get_level_88_domain(size_t parent_capacity, cbl_field_t *var, size_t &returned_size)
  {
  if( var->type != FldClass || var->level != 88 )
    {
    returned_size = 0;
    return NULL;
    }

  // Entering here means we know that this is FldClass of level 88

  // We convert the incoming information at var->data.domains to a single
  // stream of bytes.  We return a malloced pointer to that stream; returned
  // size is the size of the stream.

  // The nature of an 88 is that each element is a pair

  // The following pairs are zero-terminated strings.  It thus
  // follows that the strings cannot contain '\0' characters.

  // Each element of the pair is converted to a stream:
  // For strings of bytes:
  //       ddd A <ddd bytes>
  // For figurative constants:
  //       1Fx, where x is in [LZSQH], for LOW-VALUE ZERO SPACE QUOTE HIGH-VALUE

  // Numerics are converted to strings, and handled as above

  size_t retval_capacity = 64;
  char *retval = (char *)xmalloc(retval_capacity);
  size_t output_index = 0;

  // Loop through the provided domains:
  returned_size = 0;
  const struct cbl_domain_t *domain = var->data.domain_of();
  while( domain->first.name() )
    {
    // We have another pair to process
    size_t stream_len;
    char *stream;

    // Do the first element of the domain
    stream = level_88_helper(parent_capacity, domain->first, stream_len);
    if( output_index + stream_len > retval_capacity )
      {
      retval_capacity *= 2;
      retval = (char *)xrealloc(retval, retval_capacity);
      }
    memcpy(retval + output_index, stream, stream_len);
    output_index += stream_len;
    returned_size += stream_len;
    free(stream);

    // Do the second element of the domain
    stream = level_88_helper(parent_capacity, domain->last, stream_len);
    if( output_index + stream_len > retval_capacity )
      {
      retval_capacity *= 2;
      retval = (char *)xrealloc(retval, retval_capacity);
      }
    memcpy(retval + output_index, stream, stream_len);
    output_index += stream_len;
    returned_size += stream_len;
    free(stream);
    domain += 1;
    }
  retval[returned_size++] = '\0';
  return retval;
  }

static
char *
get_class_condition_string(cbl_field_t *var)
  {
  // We know at this point that var is FldClass
  // The LEVEL is not 88, so this is a CLASS SPECIAL-NAME

  const struct cbl_domain_t *domain = var->data.domain_of();

  /*  There are five possibilities we need to deal with.

      66
      66 THROUGH 91
      91 THROUGH 66   // This is the same as 66 THROUGH 91
      "A"
      "A" THROUGH "Z
      "Z" THROUGH "A" // This is the same as "A" THROUGH "Z"
      "ABCJ12"        // This is the same as "A" "B" "C" ...

      Expressly presented numbers are the ordinal positions in the run-time
      character set.  So, an ASCII "A" would be given as 66, which is one
      greater than 65, which is the ASCII codepoint for "A".  An EBCDIC "A"
      would be presented as 194, which is one greater than 193, which is the
      decimal representation of an EBCDIC "A", whose hex code is 0xC2.

      We need to account for EBCDIC as well as ASCII.  In EBCDIC,
      "A" THROUGH "Z" doesn't mean what it looks like it means, because EBCIDC
      encoding has gaps between I and J, and between R and S.  That isn't true
      in ASCII.  We don't want to deal with these issues at compile time, so we
      are encoding numeric ordinals with their negated values, while other
      characters are given as the numeric forms of their ASCII encoding.
      Conversion to EBCDIC occurs at runtime.

      In support of this strategy, character strings like "ABCD" are broken up
      into "A" "B" "C" "D" and converted to their hexadecimal representations.
      */

  char ach[8192];
  memset(ach, 0, sizeof(ach));
  char *p = ach;

  while( domain->first.is_numeric || domain->first.name() )
    {
    // *What* were they smoking back then?

    uint8_t value1;
    uint8_t value2;

    char achFirstName[256];
    char achLastName[256];

    size_t first_name_length = domain->first.size()
                              ? domain->first.size()
                              : strlen(domain->first.name());
    size_t last_name_length = domain->last.size()
                              ? domain->last.size()
                              : strlen(domain->last.name());

    if( domain->first.is_numeric )
      {
      if( strlen(ach) > sizeof(ach) - 1000  )
        {
        cbl_internal_error("Nice try, but you can't fire me. I quit!");
        }

      // We are working with unquoted strings that contain the values 1 through
      // 256:
      value1 = (uint8_t)atoi(domain->first.name());
      value2 = (uint8_t)atoi(domain->last.name());
      if( value2 < value1 )
        {
        std::swap(value1, value2);
        }
      if( value1 != value2  )
        {
        p += sprintf(p, "-%2.2X/-%2.2X ", value1-1, value2-1);
        }
      else
        {
        p += sprintf(p, "-%2.2X ", value1-1);
        }
      }
    else if( first_name_length == 1 )
      {
      // Since the first.name is a single character, we can do this as
      // a single-character pair.

      // Keep in mind that the single character might be a two-byte UTF-8
      // codepoint
        uint8_t ch1 = domain->first.name()[0];
        uint8_t ch2 = domain->last.name()[0];

        gcc_assert(first_name_length <= 2);
        gcc_assert(last_name_length <= 2);

      char *p2;
      size_t one;
      p2 = achFirstName;
      one = 8;
      raw_to_internal(&p2, &one, domain->last.name(), last_name_length);
      ch2 = achFirstName[0];

      p2 = achLastName;
      one = 8;
      raw_to_internal(&p2, &one, domain->first.name(), first_name_length);
      ch1 = achLastName[0];

      if( ch1 < ch2 )
        {
        value1 = ch1;
        value2 = ch2;
        }
      else
        {
        value2 = ch1;
        value1 = ch2;
        }
      if( value1 != value2  )
        {
        p += sprintf(p, "%2.2X/%2.2X ", value1, value2);
        }
      else
        {
        p += sprintf(p, "%2.2X ", value1);
        }
      }
    else
      {
      gcc_assert( first_name_length > 1 );

      // We are working with a string larger than 1 character.  The COBOL
      // spec says there can't be a THROUGH, so we ignore the last.name:
      char *p2;
      size_t one;
      p2 = achFirstName;
      one = 8;
      raw_to_internal(&p2, &one, domain->last.name(), last_name_length);

      for(size_t i=0; i<first_name_length; i++)
        {
        p += sprintf(p, "%2.2X ", (unsigned char)achFirstName[i]);
        }
      }
    domain += 1;
    }

  // Wipe out the trailing space
  ach[strlen(ach)-1] = '\0';
  char *retval = xstrdup(ach);

  return retval;
  }

struct program_reference_t {
  size_t caller;
  const char *called;

  program_reference_t( size_t caller, const char called[] )
    : caller(caller), called(xstrdup(called))
  {}
  bool operator==( const program_reference_t& that ) const {
    return caller == that.caller && 0 == strcasecmp(called, that.called);
  }
  bool operator<( const program_reference_t& that ) const {
    if( caller == that.caller ) return 0 < strcasecmp(called, that.called);
    return caller < that.caller;
  }
};

struct called_tree_t {
  tree node;
  cbl_call_convention_t convention;

  called_tree_t( tree node,
                 cbl_call_convention_t convention )
    : node(node), convention(convention)
  {}
  bool operator==( const called_tree_t& that ) const {
    return node == that.node && convention == that.convention;
  }

  class match_tree {  // match node regardless of convention
    tree node;

   public:
    match_tree( tree node ) : node(node) {}
    bool operator()( const called_tree_t& that ) const {
      return this->node == that.node;
    }
  };
};

static std::map<program_reference_t, std::list<called_tree_t> > call_targets;
static std::map<tree, cbl_call_convention_t> called_targets;

static void
parser_call_target( tree func )
  {
    cbl_call_convention_t convention = current_call_convention();
    const char *name = IDENTIFIER_POINTER( DECL_NAME(func) );
    program_reference_t key(current_program_index(), name);

    // Each func is unique and inserted only once.
    assert( called_targets.find(func) == called_targets.end() );
    called_targets[func] = convention;

    called_tree_t value(func, convention);
    auto& p = call_targets[key];
    p.push_back(value);
  }

/*
 * Is the node a recorded call target?  The language-dependent
 * function cobol_set_decl_assembler_name will lower-case the name
 * unless, for a specific call, this function returns
 * cbl_call_verbatim_e.
 */
cbl_call_convention_t
parser_call_target_convention( tree func )
  {
    auto p = called_targets.find(func);
    if( p != called_targets.end() ) return p->second;

    return cbl_call_cobol_e;
  }

void
parser_call_targets_dump()
  {
    dbgmsg( "call targets for #%zu", current_program_index() );
    for( const auto& elem : call_targets ) {
      const auto& k = elem.first;
      const auto& v = elem.second;
      fprintf(stderr, "\t#%-3zu %s calls %s ",
              k.caller, cbl_label_of(symbol_at(k.caller))->name, k.called);
      char ch = '[';
      for( auto func : v ) {
        fprintf( stderr, "%c %s", ch, IDENTIFIER_POINTER(DECL_NAME(func.node)) );
        ch = ',';
      }
      fprintf(stderr, " ]\n");
    }
  }

size_t
parser_call_target_update( size_t caller,
                           const char plain_name[],
                           const char mangled_name[] )
  {
    auto key = program_reference_t(caller, plain_name);
    auto p = call_targets.find(key);
    if( p == call_targets.end() ) return 0;

    for( auto func : p->second )
      {
      func.convention = cbl_call_verbatim_e;
      DECL_NAME(func.node) = get_identifier(mangled_name);
      }
    return p->second.size();
  }

static tree
function_handle_from_name(cbl_refer_t &name,
                          tree function_return_type)
  {
  Analyze();

  tree function_type = build_varargs_function_type_array(
                        function_return_type,
                        0,
                        NULL);
  tree function_pointer = build_pointer_type(function_type);
  tree function_handle  = gg_define_variable(function_pointer, "..function_handle.1", vs_stack);

  if( name.field->type == FldPointer )
    {
    // If the parameter is a pointer, just pick up the value and head for the
    // exit
    if( refer_is_clean(name) )
      {
      gg_memcpy(gg_get_address_of(function_handle),
                member(name.field->var_decl_node, "data"),
                sizeof_pointer);
      }
    else
      {
      gg_memcpy(gg_get_address_of(function_handle),
                qualified_data_source(name),
                sizeof_pointer);
      }
    return function_handle;
    }
  else if( use_static_call() && is_literal(name.field) )
    {
    // It's a literal, and we are using static calls. Generate the CALL, and
    // pass the address expression to parser_call_target().  That will cause
    // parser_call_target_update() to replace any nested CALL "foo" with the
    // local "foo.60" name.

    // We create a reference to it, which is later resolved by the linker.
    tree addr_expr = gg_get_function_address( function_return_type,
                                              name.field->data.initial);
    gg_assign(function_handle, addr_expr);

    tree func = TREE_OPERAND(addr_expr, 0);
    parser_call_target(func); // add function to list of call targets
    }
  else
    {
    // This is not a literal or static
    if( name.field->type == FldLiteralA )
      {
      gg_assign(function_handle,
                gg_cast(build_pointer_type(function_type),
                        gg_call_expr(VOID_P,
                                    "__gg__function_handle_from_literal",
                                    build_int_cst_type(INT, current_function->our_symbol_table_index),
                                    gg_string_literal(name.field->data.initial),
                                    NULL_TREE)));
      }
    else
      {
      gg_assign(function_handle,
                gg_cast(build_pointer_type(function_type),
                        gg_call_expr( VOID_P,
                                      "__gg__function_handle_from_name",
                                      build_int_cst_type(INT, current_function->our_symbol_table_index),
                                      gg_get_address_of(name.field->var_decl_node),
                                      refer_offset_source(name),
                                      refer_size_source(  name),
                                      NULL_TREE)));
      }
    }

  return function_handle;
  }

void
parser_initialize_programs(size_t nprogs, struct cbl_refer_t *progs)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" ")
    for( size_t i=0; i<nprogs; i++)
      {
      if( i > 0 )
        {
        SHOW_PARSE_INDENT
        }
      if( progs[i].field->type == FldLiteralA )
        {
        SHOW_PARSE_TEXT("\"")
        SHOW_PARSE_TEXT(progs[i].field->data.initial)
        SHOW_PARSE_TEXT("\"")
        }
      else
        {
        SHOW_PARSE_TEXT("")
        SHOW_PARSE_TEXT(progs[i].field->name)
        }
      }
    SHOW_PARSE_END
    }

  for( size_t i=0; i<nprogs; i++ )
    {
    tree function_handle = function_handle_from_name( progs[i],
                                                      COBOL_FUNCTION_RETURN_TYPE);
    gg_call(VOID,
            "__gg__to_be_canceled",
            gg_cast(SIZE_T, function_handle),
            NULL_TREE);
    }
  }

void parser_statement_begin()
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    char ach[64];
    snprintf  (ach, sizeof(ach),
              " yylineno %d first/last %d/%d",
              yylineno,
              cobol_location().first_line,
              cobol_location().last_line );
    SHOW_PARSE_TEXT(ach);
    SHOW_PARSE_END
    }


  if( gg_get_current_line_number() == DEFAULT_LINE_NUMBER )
    {
    // This code is prevents anomolies when the first line of a program is
    // a PERFORM <proc> ... TEST AFTER ... UNTIL ...
    gg_set_current_line_number(CURRENT_LINE_NUMBER-1);
    gg_assign(var_decl_nop, build_int_cst_type(INT, 106));
    }

  gg_set_current_line_number(CURRENT_LINE_NUMBER);
  }

static void
initialize_variable_internal( cbl_refer_t refer,
                              bool explicitly=false,
                              bool just_once=false)
  {
  // fprintf(stderr, "initialize_variable_internal for %s\n", refer.field->name);
  // gg_printf("initialize_variable_internal for %s\n",
            // gg_string_literal(refer.field->name),
            // NULL_TREE);
  cbl_field_t *parsed_var = refer.field;

  if( parsed_var->type == FldLiteralA )
    {
    return;
    }

  if( parsed_var->is_key_name() )
    {
    // This field is actually a placeholder for a RECORD KEY alias.  It didn't
    // go through parser_symbol_add(), and so any attempt to initialize it
    // results in an error because there is no var_decl_node.
    return;
    }

  if( is_register_field( parsed_var) )
    {
    return;
    }

  if( parsed_var && parsed_var->type == FldBlob )
    {
    return;
    }

  Analyze();
  SHOW_PARSE
    {
    do
      {
      fprintf(  stderr,
                "( %d ) %s():",
                CURRENT_LINE_NUMBER,
                __func__);
      }
    while(0);
    SHOW_PARSE_REF(" ", refer);
    if( parsed_var->data.initial )
      {
      SHOW_PARSE_TEXT(" >>")
      if( parsed_var->level == 88)
        {
        size_t returned_size = 0;
        char *string88 = get_level_88_domain(0, parsed_var, returned_size);

        char *p = string88;
        bool first = true;
        while(*p)
          {
          char *pend;
          size_t length1 = strtoull(p, &pend, 10);
          char *string1  = pend + 1;
          char flag = *pend;
          p = string1 + length1;
          if(flag == 'A' )
            {
            char ach2[] = "x";
            SHOW_PARSE_TEXT("\"")
            for(size_t i=0; i<length1; i++)
              {
              ach2[0] = string1[i];
              SHOW_PARSE_TEXT(ach2)
              }
            SHOW_PARSE_TEXT("\"")
            }
          else
            {
            switch(string1[0])
              {
              case 'L':
                SHOW_PARSE_TEXT("LOW-VALUE")
                break;
              case 'Z':
                SHOW_PARSE_TEXT("ZERO")
                break;
              case 'S':
                SHOW_PARSE_TEXT("SPACE")
                break;
              case 'Q':
                SHOW_PARSE_TEXT("QUOTE")
                break;
              case 'H':
                SHOW_PARSE_TEXT("HIGH-VALUE")
                break;
              default:
                SHOW_PARSE_TEXT("???")
                break;
              }
            }
          if( first )
            {
            SHOW_PARSE_TEXT("/")
            }
          else
            {
            if(*p)
              {
              SHOW_PARSE_TEXT(" ")
              }
            }
          first = !first;
          }
        free(string88);
        }
      else if( parsed_var->type == FldClass )
        {
        char *p = get_class_condition_string(parsed_var);
        SHOW_PARSE_TEXT(p);
        free(p);
        }
      else
        {
        switch(parsed_var->type)
          {
          case FldGroup:
          case FldAlphanumeric:
          case FldNumericEdited:
          case FldAlphaEdited:
          case FldLiteralA:
            SHOW_PARSE_TEXT(parsed_var->data.initial);
            break;
          default:
            {
            char ach[128];
            real_to_decimal (ach,
                             TREE_REAL_CST_PTR (parsed_var->data.value_of()),
                             sizeof(ach), 16, 0);
            SHOW_PARSE_TEXT(ach);
            break;
            }
          }

        }
      SHOW_PARSE_TEXT("<<")
      }
    SHOW_PARSE_END
    }

  CHECK_FIELD(parsed_var);

  // When initializing a variable, we have to ignore any DEPENDING ON clause
  // that might otherwise apply
  suppress_dest_depends = true;

  bool is_redefined = false;

  cbl_field_t *family_tree = parsed_var;
  while(family_tree)
    {
    if( symbol_redefines(family_tree) )
      {
      is_redefined = true;
      break;
      }

    family_tree = parent_of(family_tree);
    }

  if( parsed_var->level == 66 )
    {
    // Treat RENAMES as if they are redefines:
    is_redefined = true;
    }

  if( parsed_var->data.initial )
    {
    bool a_parent_initialized = false;
    cbl_field_t *parent = parent_of(parsed_var);
    while( parent )
      {
      if( parent->attr & has_value_e )
        {
        a_parent_initialized = true;
        break;
        }
      parent = parent_of(parent);
      }
    if( !a_parent_initialized )
      {
      parsed_var->attr |= has_value_e;
      }
    }

  static const int DEFAULT_BYTE_MASK = 0x00000000FF;
  static const int NSUBSCRIPT_MASK   = 0x0000000F00;
  static const int NSUBSCRIPT_SHIFT  =            8;
  static const int DEFAULTBYTE_BIT   = 0x0000001000;
  static const int EXPLICIT_BIT      = 0x0000002000;
  static const int REDEFINED_BIT     = 0x0000004000;
  static const int JUST_ONCE_BIT     = 0x0000008000;

  int flag_bits  = 0;
  flag_bits     |= explicitly ? EXPLICIT_BIT : 0;
  flag_bits     |= is_redefined && !explicitly ? REDEFINED_BIT : 0 ;
  flag_bits     |=  wsclear()
                    ? DEFAULTBYTE_BIT + (*wsclear() & DEFAULT_BYTE_MASK)
                    : 0;
  flag_bits     |= (refer.nsubscript << NSUBSCRIPT_SHIFT) & NSUBSCRIPT_MASK;
  flag_bits     |= just_once ? JUST_ONCE_BIT : 0 ;

  suppress_dest_depends = false;  // Set this to false so that refer_is_clean is valid
  //fprintf(stderr, "refer_is_clean %2.2d %s %d 0x%lx\n", refer.field->level, refer.field->name, refer_is_clean(refer), refer.field->attr);

  if( !refer_is_clean(refer) )
    {
    gg_call(VOID,
            "__gg__initialize_variable",
            gg_get_address_of(refer.field->var_decl_node),
            refer_offset_dest(refer),
            build_int_cst_type(INT, flag_bits),
            NULL_TREE);
    }
  else
    {
    // We have a clean refer with no mods, so we can send just the pointer to
    // the field
    gg_call(VOID,
            "__gg__initialize_variable_clean",
            gg_get_address_of(refer.field->var_decl_node),
            build_int_cst_type(INT, flag_bits) ,
            NULL_TREE);
    }

  suppress_dest_depends = true;

  TRACE1
    {
    TRACE1_HEADER
    if( refer.field->level )
      {
      gg_fprintf( trace_handle,
                  1, "%2.2d ",
                  build_int_cst_type(INT, refer.field->level));
      }
    TRACE1_REFER_INFO("", refer)
    if( refer.field->level == 88 )
      {
      TRACE1_TEXT(" [");

      size_t returned_size = 0;
      char *string88 = get_level_88_domain(0, parsed_var, returned_size);

      char *p = string88;
      bool first = true;
      while(*p)
        {
        char *pend;
        size_t length1 = strtoull(p, &pend, 10);
        char *string1  = pend + 1;
        char flag = *pend;
        p = string1 + length1;
        if( flag == 'A' )
          {
          char ach2[] = "x";
          TRACE1_TEXT("\"")
          for(size_t i=0; i<length1; i++)
            {
            ach2[0] = string1[i];
            TRACE1_TEXT(ach2)
            }
          TRACE1_TEXT("\"")
          }
        else
          {
          switch(string1[0])
            {
            case 'L':
              TRACE1_TEXT("LOW-VALUE")
              break;
            case 'Z':
              TRACE1_TEXT("ZERO")
              break;
            case 'S':
              TRACE1_TEXT("SPACE")
              break;
            case 'Q':
              TRACE1_TEXT("QUOTE")
              break;
            case 'H':
              TRACE1_TEXT("HIGH-VALUE")
              break;
            default:
              TRACE1_TEXT("???")
              break;
            }
          }
        if( first )
          {
          TRACE1_TEXT("/")
          }
        else
          {
          if(*p)
            {
            TRACE1_TEXT(" ")
            }
          }
        first = !first;
        }
      free(string88);
      TRACE1_TEXT("] ");
      }
    else if( parsed_var->type == FldClass )
      {
      char *p = get_class_condition_string(parsed_var);
      TRACE1_TEXT(p);
      free(p);
      }
    else
      {
      TRACE1_FIELD_VALUE("", parsed_var, "")
      }
    TRACE1_END
    }
  suppress_dest_depends = false;
  }

//static void
//initialize_variable_internal( cbl_field_t *field,
//                              bool explicitly=false,
//                              bool just_once=false)
//  {
//  cbl_refer_t wrapper(field);
//  initialize_variable_internal( wrapper,
//                                explicitly,
//                                just_once);
//  }

void
parser_initialize(cbl_refer_t refer, bool like_parser_symbol_add)
  {
  //gg_printf("parser_initialize %s\n", gg_string_literal(refer.field->name), NULL_TREE);
  if( like_parser_symbol_add )
    {
    initialize_variable_internal(refer);
    }
  else
    {
    gcc_assert(refer.field->data.initial);
    static const bool explicitly = true;
    initialize_variable_internal(refer, explicitly);
    }
  }

static void
get_binary_value_from_float(tree         value,
                            cbl_refer_t &dest,
                            cbl_field_t *source,
                            tree         source_offset
                            )
  {
  // The destination is something with rdigits; the source is FldFloat
  tree ftype;
  switch( source->data.capacity )
    {
    case 4:
      ftype = FLOAT;
      break;
    case 8:
      ftype = DOUBLE;
      break;
    case 16:
      ftype = FLOAT128;
      break;
    default:
      gcc_unreachable();
      break;
    }
  tree fvalue = gg_define_variable(ftype);
  gg_assign(fvalue,
            gg_indirect(gg_cast(build_pointer_type(ftype),
                                gg_add( member(source->var_decl_node,"data"),
                                        source_offset))));

  // We need to convert the floating point value to an integer value with the
  // rdigits lined up properly.

  int rdigits = get_scaled_rdigits( dest.field );
  gg_assign(fvalue,
            gg_multiply(fvalue,
                        gg_float(ftype,
                                 wide_int_to_tree(INT,
                                                  get_power_of_ten(rdigits)))));

  // And we need to throw away any digits to the left of the leftmost digits:
  // At least, we need to do so in principl.  I am deferring this problem until
  // I understand it better.

  // We now have a floating point value that has been multiplied by 10**rdigits
  gg_assign(value, gg_trunc(TREE_TYPE(value), fvalue));
  }

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
static void
gg_attribute_bit_clear(struct cbl_field_t *var, cbl_field_attr_t bits)
  {
  gg_assign(  member(var, "attr"),
              gg_bitwise_and( member(var, "attr"),
                              gg_bitwise_not( build_int_cst_type(SIZE_T, bits) )));
  }

static
tree
gg_attribute_bit_get(struct cbl_field_t *var, cbl_field_attr_t bits)
  {
  tree retval = gg_bitwise_and( member(var, "attr"),
                                build_int_cst_type(SIZE_T, bits) );
  return retval;
  }

static void
gg_attribute_bit_set(struct cbl_field_t *var, cbl_field_attr_t bits)
  {
  gg_assign(  member(var, "attr"),
              gg_bitwise_or(member(var, "attr"),
                            build_int_cst_type(SIZE_T, bits)));
  }
#pragma GCC diagnostic pop

static void
gg_default_qualification(struct cbl_field_t * /*var*/)
  {
//  gg_attribute_bit_clear(var, refmod_e);
  }

static void
gg_get_depending_on_value(tree depending_on, cbl_field_t *current_sizer)
  {
  // We have to deal with the possibility of a DEPENDING_ON variable,
  // and we have to apply array bounds whether or not there is a DEPENDING_ON
  // variable:

  tree occurs_lower = gg_define_variable(LONG, "_lower");
  tree occurs_upper = gg_define_variable(LONG, "_upper");

  gg_assign(occurs_lower, build_int_cst_type(LONG, current_sizer->occurs.bounds.lower));
  gg_assign(occurs_upper, build_int_cst_type(LONG, current_sizer->occurs.bounds.upper));

  if( current_sizer->occurs.depending_on )
    {
    // Get the current value of the depending_on data-item:
    tree value = gg_define_int128();
    get_binary_value( value,
                      NULL,
                      cbl_field_of(symbol_at(current_sizer->occurs.depending_on)),
                      size_t_zero_node);
    gg_assign(depending_on, gg_cast(LONG, value));
    IF( depending_on, lt_op, occurs_lower )
    // depending_is can be no less than occurs_lower:
      gg_assign(depending_on, occurs_lower );
    ELSE
      ENDIF
    IF( depending_on, gt_op, occurs_upper )
    // depending_is can be no greater than occurs_upper:
      gg_assign(depending_on, occurs_upper );
    ELSE
      ENDIF
    }
  else
    {
    gg_assign(depending_on, occurs_upper);
    }
  }

static int
digits_to_bytes(int digits)
  {
  int retval;
  if( digits <= 2 )
    {
    retval = 1;
    }
  else if( digits <= 4 )
    {
    retval = 2;
    }
  else if( digits <= 9 )
    {
    retval = 4;
    }
  else if( digits <= 18 )
    {
    retval = 8;
    }
  else
    {
    retval = 16;
    }
  return retval;
  }

static size_t
get_bytes_needed(cbl_field_t *field)
  {
  size_t retval = 0;
  switch(field->type)
    {
    case FldIndex:
    case FldPointer:
    case FldFloat:
    case FldLiteralN:
      retval = field->data.capacity;
      break;

    case FldNumericDisplay:
      {
      int digits;
      if( field->attr & scaled_e && field->data.rdigits<0)
        {
        digits = field->data.digits + -field->data.rdigits;
        }
      else
        {
        digits = field->data.digits;
        }
      retval = digits_to_bytes(digits);
      break;
      }

    case FldPacked:
      {
      int digits;
      if( field->attr & scaled_e && field->data.rdigits<0)
        {
        digits = field->data.digits + -field->data.rdigits;
        }
      else
        {
        digits = field->data.digits;
        }
      if( !(field->attr & separate_e) )
        {
        // This is COMP-3, so there is a sign nybble.
        digits += 1;
        }
      retval = (digits+1)/2;
      break;
      }

    case FldNumericBinary:
    case FldNumericBin5:
      {
      if( field->data.digits )
        {
        int digits;
        if( field->attr & scaled_e && field->data.rdigits<0)
          {
          digits = field->data.digits + -field->data.rdigits;
          }
        else
          {
          digits = field->data.digits;
          }
        retval = digits_to_bytes(digits);
        }
      else
        {
        retval = field->data.capacity;
        }
      break;
      }

    default:
      cbl_internal_error("%s(): Knows not the variable type %s for %s",
              __func__,
              cbl_field_type_str(field->type),
              field->name );
      break;
    }
  return retval;
  }

static void
normal_normal_compare(bool debugging,
                      tree return_int,
                      cbl_refer_t *left_side_ref,
                      cbl_refer_t *right_side_ref,
                      tree left_side,
                      tree right_side )
  {
  Analyze();

  // If a value is intermediate_e, then the rdigits can vary at run-time, so
  // we can't rely on the compile-time rdigits.

  bool left_intermediate  = (left_side_ref->field->attr & intermediate_e);
  bool right_intermediate = (right_side_ref->field->attr & intermediate_e);

  if( debugging )
    {
    gg_printf("normal_normal_compare(): left_intermediate/right_intermediate %d/%d\n",
              left_intermediate ? integer_one_node : integer_zero_node ,
              right_intermediate ? integer_one_node : integer_zero_node ,
              NULL_TREE);
    }

  bool needs_adjusting;
  if( !left_intermediate && !right_intermediate )
    {
    // Yay!  Both sides have fixed rdigit values.

    // Flag needs_adjusting as false, because we are going to do it here:
    needs_adjusting = false;
    int adjust =   get_scaled_rdigits(left_side_ref->field)
                 - get_scaled_rdigits(right_side_ref->field);

    if( adjust > 0 )
      {
      // We need to make right_side bigger to match the scale of left_side
      scale_by_power_of_ten_N(right_side, adjust);
      }
    else if( adjust < 0 )
      {
      // We need to make left_side bigger to match the scale of right_side
      scale_by_power_of_ten_N(left_side, -adjust);
      }
    }
  else
    {
    // At least one side is right_intermediate

    tree adjust;
    if( !left_intermediate && right_intermediate )
      {
      // left is fixed, right is intermediate
      adjust = gg_define_int();
      gg_assign(adjust,
                build_int_cst_type( INT,
                                    get_scaled_rdigits(left_side_ref->field)));

      gg_assign(adjust,
                gg_subtract(adjust,
                            gg_cast(INT,
                                    member(right_side_ref->field->var_decl_node,
                                           "rdigits"))));
      needs_adjusting = true;
      }
    else if( left_intermediate && !right_intermediate )
      {
      // left is intermediate, right is fixed
      adjust = gg_define_int();
      gg_assign(adjust, gg_cast(INT, member(left_side_ref->field, "rdigits")));
      gg_assign(adjust,
                gg_subtract(adjust,
                            build_int_cst_type( INT,
                                   get_scaled_rdigits(right_side_ref->field))));
      needs_adjusting = true;
      }
    else // if( left_intermediate && right_intermediate )
      {
      // Both sides are intermediate_e
      adjust = gg_define_int();
      gg_assign(adjust, gg_cast(INT, member(left_side_ref->field, "rdigits")));
      gg_assign(adjust,
                gg_subtract(adjust,
                            gg_cast(INT,
                                    member(right_side_ref->field, "rdigits"))));
      needs_adjusting = true;
      }

    if( needs_adjusting )
      {
      if( debugging )
        {
        gg_printf("normal_normal_compare(): The value of adjust is %d\n",
                  adjust,
                  NULL_TREE);
        }
      IF( adjust, gt_op, integer_zero_node )
        {
        // The right side needs to be scaled up
        scale_by_power_of_ten(right_side, adjust);
        }
      ELSE
        {
        IF( adjust, lt_op, integer_zero_node )
          {
          // The left side needs to be scaled up
          scale_by_power_of_ten(left_side, gg_negate(adjust));
          }
        ELSE
          ENDIF
        }
        ENDIF
      }
    }

  if( TREE_TYPE(left_side) != TREE_TYPE(right_side) )
    {
    // One is signed, the other isn't:
    if( left_side_ref->field->attr & signable_e )
      {
      // The left side can be negative.  If it is, the return value has to be
      // -1 for left < right
      IF( left_side, lt_op, gg_cast(TREE_TYPE(left_side), integer_zero_node) )
        {
        if( debugging )
          {
          gg_printf("normal_normal_compare(): different types returning -1\n",
                    NULL_TREE);
          }
        gg_assign( return_int, integer_minusone_node);
        }
      ELSE
        {
        // Both sides are positive, allowing a direct comparison.
        IF( gg_cast(TREE_TYPE(right_side), left_side), lt_op, right_side )
          {
          if( debugging )
            {
            gg_printf("normal_normal_compare(): returning -1\n", NULL_TREE);
            }
          gg_assign( return_int, integer_minusone_node);
          }
        ELSE
          {
          IF( gg_cast(TREE_TYPE(right_side), left_side), gt_op, right_side)
            {
            if( debugging )
              {
              gg_printf("normal_normal_compare(): returning +1\n", NULL_TREE);
              }
            gg_assign( return_int, integer_one_node);
            }
          ELSE
            {
            if( debugging )
              {
              gg_printf("normal_normal_compare(): returning zero\n", NULL_TREE);
              }
            gg_assign( return_int, integer_zero_node);
            }
            ENDIF
          }
          ENDIF
        }
        ENDIF
      }
    else
      {
      // The right side can be negative.  If it is, the return value has to be
      // +1 for left > right
      IF( right_side, lt_op, gg_cast(TREE_TYPE(right_side), integer_zero_node) )
        {
        if( debugging )
          {
          gg_printf("normal_normal_compare(): different types returning +1\n", NULL_TREE);
          }
        gg_assign( return_int, integer_one_node);
        }
      ELSE
        {
        // Both sides are positive, allowing a direct comparison.
        IF( left_side, lt_op, gg_cast(TREE_TYPE(left_side), right_side) )
          {
          if( debugging )
            {
            gg_printf("normal_normal_compare(): returning -1\n", NULL_TREE);
            }
          gg_assign( return_int, integer_minusone_node);
          }
        ELSE
          {
          IF( left_side, gt_op, gg_cast(TREE_TYPE(left_side), right_side) )
            {
            if( debugging )
              {
              gg_printf("normal_normal_compare(): returning +1\n", NULL_TREE);
              }
            gg_assign( return_int, integer_one_node);
            }
          ELSE
            {
            if( debugging )
              {
              gg_printf("normal_normal_compare(): returning zero\n", NULL_TREE);
              }
            gg_assign( return_int, integer_zero_node);
            }
            ENDIF
          }
          ENDIF
        }
        ENDIF
      }
    }
  else
    {
    // Both sides are the same type, allowing a direct comparison.
    IF( left_side, lt_op, right_side )
      {
      if( debugging )
        {
        gg_printf("normal_normal_compare(): returning -1\n", NULL_TREE);
        }
      gg_assign( return_int, integer_minusone_node);
      }
    ELSE
      {
      IF( left_side, gt_op, right_side )
        {
        if( debugging )
          {
          gg_printf("normal_normal_compare(): returning +1\n", NULL_TREE);
          }
        gg_assign( return_int, integer_one_node);
        }
      ELSE
        {
        if( debugging )
          {
          gg_printf("normal_normal_compare(): returning zero\n", NULL_TREE);
          }
        gg_assign( return_int, integer_zero_node);
        }
        ENDIF
      }
      ENDIF
    }
  }

static void
compare_binary_binary(tree return_int,
                      cbl_refer_t *left_side_ref,
                      cbl_refer_t *right_side_ref )
  {
  Analyze();
  static const bool debugging = false;

  // We know the two sides have binary values that can be extracted.
  tree left_side;
  tree right_side;

  // Use SIZE128 when we need two 64-bit registers to hold the value.  All
  // others fit into 64-bit LONG with pretty much the same efficiency.

  size_t left_bytes_needed  = get_bytes_needed(left_side_ref->field);
  size_t right_bytes_needed = get_bytes_needed(right_side_ref->field);

  if(     left_bytes_needed >= SIZE128
      || right_bytes_needed >= SIZE128 )
    {
    if( debugging )
      {
      gg_printf("compare_binary_binary(): using int128\n", NULL_TREE);
      }

    left_side  = gg_define_int128();
    right_side = gg_define_int128();
    }
  else
    {
    if( debugging )
      {
      gg_printf("compare_binary_binary(): using int64\n", NULL_TREE);
      }
    left_side  = gg_define_variable( left_side_ref->field->attr & signable_e ? LONG : ULONG );
    right_side = gg_define_variable(right_side_ref->field->attr & signable_e ? LONG : ULONG );
    }

  //tree dummy = gg_define_int();
  static tree hilo_left  = gg_define_variable(INT, "..cbb_hilo_left",  vs_file_static);
  static tree hilo_right = gg_define_variable(INT, "..cbb_hilo_right", vs_file_static);

  get_binary_value(left_side,
                   NULL,
                   left_side_ref->field,
                   refer_offset_source(*left_side_ref),
                   hilo_left);
  get_binary_value(right_side,
                   NULL,
                   right_side_ref->field,
                   refer_offset_source(*right_side_ref),
                   hilo_right);
  IF( hilo_left, eq_op, integer_one_node )
    {
    // left side is hi-value
    IF( hilo_right, eq_op, integer_one_node )
      {
      if( debugging )
        {
        gg_printf("compare_binary_binary(): left and right are HIGH-VALUE\n", NULL_TREE);
        }
      gg_assign(return_int, integer_zero_node);
      }
    ELSE
      {
      if( debugging )
        {
        gg_printf("compare_binary_binary(): left is HIGH-VALUE\n", NULL_TREE);
        }
      gg_assign(return_int, integer_one_node);
      }
      ENDIF
    }
  ELSE
    {
    // left is not HIGH-VALUE:
    IF( hilo_left, eq_op, integer_minus_one_node )
      {
      // left side is LOW-VALUE
      IF( hilo_right, eq_op, integer_minus_one_node )
        {
        if( debugging )
          {
          gg_printf("compare_binary_binary(): left and right are LOW-VALUE\n", NULL_TREE);
          }
        gg_assign(return_int, integer_zero_node);
        }
      ELSE
        {
        // Right side is not low-value
        if( debugging )
          {
          gg_printf("compare_binary_binary(): left is LOW-VALUE\n", NULL_TREE);
          }
        gg_assign(return_int, integer_one_node);
        }
        ENDIF
      }
    ELSE
      {
      // Left side is normal
      IF( hilo_right, eq_op, integer_one_node )
        {
        if( debugging )
          {
          gg_printf("compare_binary_binary(): right is HIGH-VALUE\n", NULL_TREE);
          }
        gg_assign(return_int, integer_minus_one_node);
        }
      ELSE
        {
        IF( hilo_right, eq_op, integer_minus_one_node )
          {
          if( debugging )
            {
            gg_printf("compare_binary_binary(): right is LOW-VALUE\n", NULL_TREE);
            }
          gg_assign(return_int, integer_one_node);
          }
        ELSE
          {
          if( debugging )
            {
            gg_printf("compare_binary_binary(): left and right are normal\n", NULL_TREE);
            }
          normal_normal_compare(debugging,
                                return_int,
                                left_side_ref,
                                right_side_ref,
                                left_side,
                                right_side
                                );
          }
          ENDIF
        }
        ENDIF
      }
      ENDIF
    }
    ENDIF
  }

#define DEBUG_COMPARE

static void
cobol_compare(  tree return_int,
                cbl_refer_t &left_side_ref,
                cbl_refer_t &right_side_ref )
  {
  Analyze();
// gg_printf("cobol_compare %s %s \"%s\" \"%s\"\n",
          // gg_string_literal(left_side_ref.field->name),
          // gg_string_literal(right_side_ref.field->name),
          // member(left_side_ref.field, "data"),
          // gg_string_literal(right_side_ref.field->data.initial),
          // NULL_TREE);

  CHECK_FIELD(left_side_ref.field);
  CHECK_FIELD(right_side_ref.field);
  // This routine is in support of conditionals in the COBOL program.
  // It takes two arbitrary COBOL variables from the parser and compares them
  // according to a nightmarish set of rules.

  // See ISO/IEC 1989:2014(E) section 8.8.4.1.1 (page 153)

  // The return_int value is -1 when left_side  < right_side
  //                          0      left_side == right_side
  //                          1      left_side  > right_side

  bool compared = false;

  // In the effort to convert to in-line GIMPLE comparisons, I became flummoxed
  // by comparisons involving REFMODs.  This will have to be revisited, but for
  // now I decided to keep using the libgcobol code, which according to NIST
  // works properly.

  if(    !left_side_ref.refmod.from
      && !left_side_ref.refmod.len
      && !right_side_ref.refmod.from
      && !right_side_ref.refmod.len )
    {
    cbl_refer_t *lefty = &left_side_ref;
    cbl_refer_t *righty = &right_side_ref;

    int ntries = 1;
    while( ntries <= 2 )
      {
      switch( lefty->field->type )
        {
        case FldLiteralN:
          {
          switch( righty->field->type )
            {
            case FldLiteralN:
            case FldNumericBinary:
            case FldNumericBin5:
            case FldPacked:
            case FldNumericDisplay:
            case FldIndex:
              compare_binary_binary(return_int, lefty, righty);
              compared = true;
              break;

            case FldGroup:
            case FldAlphanumeric:
            case FldLiteralA:
              {
              // Comparing a FldLiteralN to an alphanumeric
              // It is the case that data.initial is in the original form seen
              // in the source code, which means that even in EBCDIC mode the
              // characters are in the "ASCII" state.

              static size_t buffer_size = 0;
              static char *buffer = NULL;
              raw_to_internal(&buffer,
                              &buffer_size,
                              lefty->field->data.initial,
                              strlen(lefty->field->data.initial));

              gg_assign(  return_int, gg_call_expr(
                          INT,
                          "__gg__literaln_alpha_compare",
                          gg_string_literal(buffer),
                          gg_get_address_of(righty->field->var_decl_node),
                          refer_offset_source(*righty),
                          refer_size_source(  *righty),
                          build_int_cst_type(INT,
                                        (righty->all ? REFER_T_MOVE_ALL : 0)),
                          NULL_TREE));
              compared = true;
              break;
              }

            default:
              break;
            }
          break;
          }

        case FldNumericBin5:
        case FldNumericBinary:
        case FldPacked:
        case FldNumericDisplay:
          {
          switch( righty->field->type )
            {
            case FldNumericBin5:
            case FldNumericBinary:
            case FldPacked:
            case FldNumericDisplay:
              {
              compare_binary_binary(return_int, lefty, righty);
              compared = true;
              break;
              }

            default:
              break;
            }
          break;
          }

        default:
          break;
        }
      if( compared )
        {
        break;
        }
      // We weren't able to compare left/right.  Let's see if we understand
      // right/left
      std::swap(lefty, righty);
      ntries += 1;
      }

    if( compared && ntries == 2 )
      {
      // We have a successful comparision, but we managed it on the second try,
      // which means our result has the wrong sign.  Fix it:
      gg_assign(return_int, gg_negate(return_int));
      }
    }

  if( !compared )
    {
    // None of our explicit comparisons up above worked, so we revert to the
    // general case:
    int leftflags  =   (left_side_ref.all          ? REFER_T_MOVE_ALL   : 0)
                    +  (left_side_ref.addr_of      ? REFER_T_ADDRESS_OF : 0)
                    +  (left_side_ref.refmod.from  ? REFER_T_REFMOD     : 0);
    int rightflags =   (right_side_ref.all         ? REFER_T_MOVE_ALL   : 0)
                    +  (right_side_ref.addr_of     ? REFER_T_ADDRESS_OF : 0)
                    +  (right_side_ref.refmod.from ? REFER_T_REFMOD     : 0);
    gg_assign(  return_int, gg_call_expr(
                INT,
                "__gg__compare",
                gg_get_address_of(left_side_ref.field->var_decl_node),
                refer_offset_source(left_side_ref),
                refer_size_source(  left_side_ref),
                build_int_cst_type(INT, leftflags),
                gg_get_address_of(right_side_ref.field->var_decl_node),
                refer_offset_source(right_side_ref),
                refer_size_source(  right_side_ref),
                build_int_cst_type(INT, rightflags),
                integer_zero_node,
                NULL_TREE));
    compared = true;
    }

//  gg_printf("   result is %d\n", return_int, NULL_TREE);
  }

static void
move_tree(  cbl_field_t  *dest,
            tree          offset,
            tree          psz_source,
            tree length_bump=integer_zero_node)   // psz_source is a null-terminated string
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD(" ", dest);
    SHOW_PARSE_END
    }

  bool moved = true;

  tree source_length = gg_define_size_t();
  gg_assign(source_length, gg_strlen(psz_source));
  gg_assign(source_length, gg_add(source_length, gg_cast(SIZE_T, length_bump)));

  tree min_length = gg_define_size_t();

  tree location = gg_define_uchar_star();
  tree length   = gg_define_size_t();

  gg_assign(location,
            gg_add(member(dest->var_decl_node, "data"),
                   offset));
  gg_assign(length,
            member(dest->var_decl_node, "capacity"));

  IF(source_length, lt_op, length)
    {
    gg_assign(min_length, source_length);
    }
  ELSE
    {
    gg_assign(min_length, length);
    }
  ENDIF

  tree value;
  tree rdigits;

  switch( dest->type )
    {
    case FldGroup:
    case FldAlphanumeric:
      // Space out the alphanumeric destination:
      gg_memset(  location,
                  build_int_cst_type(INT, internal_space),
                  length );
      // Copy the alphanumeric result over.
      gg_memcpy(  location,
                  psz_source,
                  min_length );
      break;

    case FldNumericDisplay:
    case FldNumericEdited:
    case FldNumericBinary:
    case FldNumericBin5:
    case FldPacked:
    case FldIndex:
      {
      value   = gg_define_int128();
      rdigits = gg_define_int();

      gg_assign(value,
                gg_call_expr( INT128,
                              "__gg__dirty_to_binary_internal",
                              psz_source,
                              source_length,
                              gg_get_address_of(rdigits),
                              NULL_TREE));

      gg_call(VOID,
              "__gg__int128_to_qualified_field",
              gg_get_address_of(dest->var_decl_node),
              offset,
              build_int_cst_type(SIZE_T, dest->data.capacity),
              value,
              rdigits,
              build_int_cst_type(INT, truncation_e),
              null_pointer_node,
              NULL_TREE);
      }
    break;

    case FldAlphaEdited:
      {
      gg_call(VOID,
              "__gg__string_to_alpha_edited_ascii",
              location,
              psz_source,
              min_length,
              member(dest->var_decl_node, "picture"),
              NULL);
      break;
      }

    default:
      moved = false;
      break;
    }

  TRACE1
    {
    TRACE1_HEADER
    gg_fprintf(trace_handle, 1, "source: \"%s\"", psz_source);
    TRACE1_END
    TRACE1_INDENT
    TRACE1_FIELD(               "dest  : ", dest, "")
    TRACE1_END
    }

  if( !moved )
    {
    dbgmsg("###### %10s in %s:%d\n", __func__, __FILE__, __LINE__ );
    cbl_internal_error( "I don't know how to MOVE an alphabetical string to %s(%s) \n",
           cbl_field_type_str(dest->type),
           dest->name
         );
    return;
    }
  }

static void
move_tree_to_field(cbl_field_t *field, tree psz)
  {
  move_tree(field, integer_zero_node, psz);
  }

static tree
get_string_from(cbl_field_t *field)
  {
  // This returns a malloced copy of either a literal string or a
  // an alphanumeric field.  The idea is that eventually free() will be
  // called in the runtime space:

  tree psz = gg_define_char_star();

  if( field )
    {
    switch( field->type )
      {
      case FldLiteralA:
        {
        gg_assign(psz,
                  gg_cast(CHAR_P,
                          gg_malloc(build_int_cst_type(SIZE_T,
                                                     field->data.capacity+1))));
        char *litstring = get_literal_string(field);
        gg_memcpy(psz,
                  gg_string_literal(litstring),
                  build_int_cst_type(SIZE_T, field->data.capacity+1));
        break;
        }

      case FldGroup:
      case FldAlphanumeric:
        // make a copy of .data:
        gg_assign(psz,
                  gg_cast(CHAR_P,
                          gg_malloc(build_int_cst_type(SIZE_T,
                                                    field->data.capacity+1))));
        gg_memcpy(  psz,
                    member(field, "data"),
                    member(field, "capacity"));
        // null-terminate it:
        gg_assign(  gg_array_value(psz, member(field, "capacity")),
                    char_nodes[0]);
        break;

      case FldForward:
        {
        // At the present time, we are assuming this happens when somebody
        // specifies an unquoted file name in an ASSIGN statement:
        //    SELECT file3 ASSIGN DISK.
        //
        // In that case, we just return DISK, which is field->name:
        psz = gg_strdup(gg_string_literal(field->name));
        break;
        }

      default:
        cbl_internal_error(
                "%s(): field->type %s must be literal or alphanumeric",
                __func__, cbl_field_type_str(field->type));
      break;
      }
    }
  else
    {
    gg_assign(psz, gg_cast(CHAR_P, null_pointer_node));
    }
  return psz;
  }

static char *
combined_name(cbl_label_t *label)
  {
  // This routine returns a pointer to a static, so make sure you use the result
  // before calling the routine again
  char *para_name     = nullptr;
  char *sect_name     = nullptr;
  const char *program_name  = current_function->our_unmangled_name;

  if( label->type == LblParagraph )
    {
    para_name = label->name;

    if( label->parent )
      {
      // It's possible for implicit
      cbl_label_t *section_label = cbl_label_of(symbol_at(label->parent));
      sect_name = section_label->name;
      }
    }
  else
    {
    sect_name = label->name;
    }

  static size_t retval_size = 256;
  static char *retval= (char *)xmalloc(retval_size);

  char *paragraph             = cobol_name_mangler(para_name);
  char *section               = cobol_name_mangler(sect_name);
  char *mangled_program_name  = cobol_name_mangler(program_name);

  while( retval_size < (paragraph ? strlen(paragraph) : 0 )
                  + (section ? strlen(section) : 0 )
                  + (mangled_program_name ? strlen(mangled_program_name) : 0 )
                  + 24 )
    {
    retval_size *= 2;
    retval = (char *)xrealloc(retval, retval_size);
    }

  *retval = '\0';
  char ach[24];
  if( paragraph )
    {
    strcat(retval, paragraph);
    }
  strcat(retval, ".");
  if( section )
    {
    strcat(retval, section);
    }
  strcat(retval, ".");
  if( mangled_program_name )
    {
    strcat(retval, mangled_program_name);
    }
  sprintf(ach, ".%ld", current_function->program_id_number);
  strcat(retval, ach);
  sprintf(ach, ".%ld", symbol_label_id(label));
  strcat(retval, ach);
  free(mangled_program_name);
  free(section);
  free(paragraph);

  return retval;
  }

// We implement SECTION and PARAGRAPH stuff before the rest of program
// structure, because we have some static routines in here that are called
// by enter_ and leave_ program, and so on.

static void
assembler_label(const char *label)
  {
  // label has to be a valid label for the assembler
  static size_t length = 0;
  static char *build = nullptr;

  const char local_text[] = ":";
  if( length < strlen(label) + strlen(local_text) + 1 )
    {
    length = strlen(label) + strlen(local_text) + 1;
    free(build);
    build = (char *)xmalloc(length);
    }

  strcpy(build, label);
  strcat(build, local_text);

  gg_insert_into_assembler(build);
  }

static void
section_label(struct cbl_proc_t *procedure)
  {
  // With nested programs, you can have multiple program/section pairs with the
  // the same names; we use a deconflictor to avoid collisions

  gg_set_current_line_number(CURRENT_LINE_NUMBER);

  size_t deconflictor = symbol_label_id(procedure->label);

  cbl_label_t *label = procedure->label;
  // The _initialize_program section isn't relevant.
  char *psz = xasprintf("%s SECTION %s in %s (%ld)",
                        ASM_COMMENT_START,
                        label->name,
                        current_function->our_unmangled_name,
                        deconflictor);
  gg_insert_into_assembler(psz);
  free(psz);

  // The label has to start with an underscore.  I tried a period, but those
  // don't seem to show up in GDB's internal symbol tables.
  char *psz2 = xasprintf( "_sect.%s",
                          combined_name(procedure->label));
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(psz2);
    SHOW_PARSE_END
    }
  assembler_label(psz2);
  free(psz2);
  gg_assign(var_decl_nop, build_int_cst_type(INT, 108));
  }

static void
paragraph_label(struct cbl_proc_t *procedure)
  {
  // We need to give each paragraph a unique and assembler-compatible name
  // that can be found and used by GDB.
  // Complications:
  //   1) paragraph names can be reused in the same program, provided they
  //      are in different sections.
  //   2) paragraph names can be duplicated in a section, provided that they
  //      are not referenced by the program.  We provide a deconflictor to
  //      separate such labels.

  gg_set_current_line_number(CURRENT_LINE_NUMBER);

  cbl_label_t *paragraph  = procedure->label;
  cbl_label_t *section    = nullptr;

  if( procedure->label->parent )
    {
    section = cbl_label_of(symbol_at(procedure->label->parent));
    }

  char *para_name    = paragraph->name;
  char *section_name = section ? section->name : nullptr;

  size_t deconflictor = symbol_label_id(procedure->label);
  
  char *psz1 = 
  xasprintf(
          "%s PARAGRAPH %s of %s in %s (%ld)",
          ASM_COMMENT_START,
          para_name ? para_name: "" ,
          section_name ? section_name: "(null)" ,
          current_function->our_unmangled_name ? current_function->our_unmangled_name: "" ,
          deconflictor );
  
  gg_insert_into_assembler(psz1);

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(psz1);
    SHOW_PARSE_END
    }
  free(psz1);

  // The label has to start with an underscore.  I tried a period, but those
  // don't seem to show up in GDB's internal symbol tables.
  char *psz2 = xasprintf( "_para.%s",
                          combined_name(procedure->label));
  assembler_label(psz2);
  free(psz2);
  gg_assign(var_decl_nop, build_int_cst_type(INT, 109));
  }

static void
pseudo_return_push(cbl_proc_t *procedure, tree return_addr)
  {
  // Put the return address onto the stack:
  //gg_suppress_location(true);

  TRACE1
    {
    TRACE1_HEADER
    gg_printf("%s %p %p",
              gg_string_literal(procedure->label->name),
              gg_cast(SIZE_T, procedure->exit.addr),
              return_addr,
              NULL_TREE);
    TRACE1_END
    }

  gg_call(VOID,
          "__gg__pseudo_return_push",
          procedure->exit.addr,
          return_addr,
          NULL_TREE);

  //gg_suppress_location(false);
  }

static void
pseudo_return_pop(cbl_proc_t *procedure)
  {
  //gg_suppress_location(true);

  TRACE1
    {
    TRACE1_HEADER
    gg_printf("%s comparing proc_exit %p to global_exit %p -- ",
              gg_string_literal(procedure->label->name),
              gg_cast(SIZE_T, procedure->exit.addr),
              var_decl_exit_address,
              NULL_TREE);
    }

  IF( var_decl_exit_address, eq_op, procedure->exit.addr )
    {
    TRACE1
      {
      TRACE1_TEXT("Returning")
      }
    // The top of the stack is us!

    // Pick up the return address from the pseudo_return stack:
    gg_assign(current_function->void_star_temp,
              gg_call_expr( VOID_P,
                            "__gg__pseudo_return_pop",
                            NULL_TREE));
    // And do the return:
    gg_goto(current_function->void_star_temp);
    }
  ELSE
    {
    TRACE1
      {
      TRACE1_TEXT("No match")
      }
    ENDIF
    }
  TRACE1
    {
    TRACE1_END
    }
  //gg_suppress_location(false);
  }

static void
leave_procedure(struct cbl_proc_t *procedure, bool /*section*/)
  {
  if(procedure)
    {
    // fprintf(stderr, "LeavingProcedure: (%p) %s %p %p %p %p %p %p\n",
    // procedure,
    // procedure->name,
    // procedure->top.go_to,
    // procedure->top.label,
    // procedure->exit.go_to,
    // procedure->exit.label,
    // procedure->bottom.go_to,
    // procedure->bottom.label);
    // Procedure can be null, for example at the beginning of a
    // new program, or after somebody else has cleared it out.
    gg_append_statement(procedure->exit.label);

    char *psz;
    psz = xasprintf("_procret.%ld:",
                    symbol_label_id(procedure->label));
    gg_insert_into_assembler(psz);
    free(psz);
    pseudo_return_pop(procedure);
    gg_append_statement(procedure->bottom.label);
    }
  }

static void
leave_section_internal()
  {
  Analyze();
  SHOW_PARSE
    {
    if(gg_trans_unit.function_stack.size() && current_function && current_function->current_section)
      {
      SHOW_PARSE_HEADER
      SHOW_PARSE_TEXT(" ")
      SHOW_PARSE_TEXT(current_function->current_section->label->name)
      SHOW_PARSE_END
      }
    }

  if( current_function->current_section )
    {
    // gg_printf(  "Leaving section %s\n",
    // build_string_literal( strlen(current_function->current_section->label->name)+1, current_function->current_section->label->name),
    // NULL_TREE);
    TRACE1
      {
      TRACE1_HEADER
      TRACE1_TEXT_ABC("\"", current_function->current_section->label->name, "\"");
      TRACE1_END
      }
    leave_procedure(current_function->current_section, true);

    current_function->current_section = NULL;
    }
  else
    {
    //gg_printf("Somebody is leaving a section twice\n", NULL_TREE);
    }
  }

void
parser_leave_section( struct cbl_label_t */*label*/ ) {}

static void
leave_paragraph_impl()
  {
  Analyze();
  SHOW_PARSE
    {
    if(gg_trans_unit.function_stack.size() && current_function && current_function->current_paragraph)
      {
      SHOW_PARSE_HEADER
      SHOW_PARSE_TEXT(" ")
      SHOW_PARSE_TEXT(current_function->current_paragraph->label->name)
      SHOW_PARSE_END
      }
    }

  if( current_function->current_paragraph )
    {
    // gg_printf(  "Leaving paragraph %s\n",
    // build_string_literal( strlen(current_function->current_paragraph->label->name)+1, current_function->current_paragraph->label->name),
    // NULL_TREE);
    TRACE1
      {
      TRACE1_HEADER
      TRACE1_TEXT_ABC("\"", current_function->current_paragraph->label->name, "\"");
      TRACE1_END
      }
    leave_procedure(current_function->current_paragraph, false);
    current_function->current_paragraph = NULL;
    }
  else
    {
    //gg_printf("Somebody is leaving a paragraph twice\n", NULL_TREE);
    }
  }

void parser_leave_paragraph( cbl_label_t * ) {}
static inline void leave_paragraph_internal() { leave_paragraph_impl(); }

static struct cbl_proc_t *
find_procedure(cbl_label_t *label)
  {
//    SHOW_PARSE
//        {
//        SHOW_PARSE_HEADER
//        SHOW_PARSE_LABEL(" ", label)
//        SHOW_PARSE_TEXT("\n");
//        }

  cbl_proc_t *retval  = label->structs.proc;

  //  We have to cope with an oddball circumstance.  When label->entered is
  //  greater than zero, it means that a paragraph with this label has been
  //  entered and left already.  This means that a paragraph name has been
  //  defined more than once.  Had it been referenced with a GOTO or PERFORM,
  //  that would have been a syntax error.
  //
  //
  //  In this case, we need to replace the existing cbl_proc_t structure. We
  //  will be laying down labels for this second (or more) instance of
  //  parser_enter_paragraph, and we must create different labels.

  if( !retval )
    {
    static int counter=1;

    // This is a new section or paragraph; we need to create its values:
    retval = (struct cbl_proc_t *)xmalloc(sizeof(struct cbl_proc_t));
    retval->label = label;

    gg_create_goto_pair(&retval->top.go_to,
                        &retval->top.label,
                        &retval->top.addr,
                        &retval->top.decl);
    gg_create_goto_pair(&retval->exit.go_to,
                        &retval->exit.label,
                        &retval->exit.addr
                        );
    gg_create_goto_pair(&retval->bottom.go_to,
                        &retval->bottom.label,
                        &retval->bottom.addr
                        );

    // fprintf(stderr, "NewProcedure: (%p) %s %p %p %p %p %p %p\n",
    // retval,
    // retval->name,
    // retval->top.go_to,
    // retval->top.label,
    // retval->exit.go_to,
    // retval->exit.label,
    // retval->bottom.go_to,
    // retval->bottom.label);

    // If this procedure is a paragraph, and it becomes the target of
    // an ALTER statement, alter_location will be used to make that change
    char *psz = xasprintf("_%s_alter_loc_%d", label->name, counter);
    retval->alter_location = gg_define_void_star(psz, vs_static);
    free(psz);
    DECL_INITIAL(retval->alter_location) = null_pointer_node;

    counter +=1 ;

    label->structs.proc = retval;
    }

  return retval;
  }

void
parser_enter_section(cbl_label_t *label)
  {
  Analyze();
  // Do the leaving before the SHOW_PARSE; it makes the output more sensible
  // A new section ends the current paragraph:
  leave_paragraph_internal();

  // And the current section:
  leave_section_internal();

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_LABEL(" ", label)
    SHOW_PARSE_END
    }

  CHECK_LABEL(label);

  // This NOP is needed to give GDB a line number for the entry point of
  // paragraphs
  gg_set_current_line_number(CURRENT_LINE_NUMBER);
  gg_assign(var_decl_nop, build_int_cst_type(INT, 101));

  struct cbl_proc_t *procedure = find_procedure(label);
  gg_append_statement(procedure->top.label);
  section_label(procedure);
  current_function->current_section = procedure;

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_LABEL("\"", label, "\"")
    TRACE1_END
    }
  }

void
parser_enter_paragraph(cbl_label_t *label)
  {
  Analyze();
  // Do the leaving before the SHOW_PARSE; the output makes more sense that way
  // A new paragraph ends the current paragraph:
  leave_paragraph_internal();

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_LABEL(" ", label)
    SHOW_PARSE_END
    }

  CHECK_LABEL(label);

  struct cbl_proc_t *procedure = find_procedure(label);
  gg_append_statement(procedure->top.label);
  paragraph_label(procedure);
  current_function->current_paragraph = procedure;

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_LABEL("\"", label, "\"")
    TRACE1_END
    }
  }

void
parser_exit_section(void)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT_ABC("\"", current_function->current_section->label->name, "\"")
    TRACE1_END
    }
  gg_append_statement(current_function->current_section->exit.go_to);
  }

void
parser_exit_paragraph(void)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT_ABC("\"", current_function->current_paragraph->label->name, "\"")
    TRACE1_END
    }
  gg_append_statement(current_function->current_paragraph->exit.go_to);
  }

void
parser_exit_perform(struct cbl_perform_tgt_t *tgt, bool cycle)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }
  if(cycle)
    {
    gg_append_statement(tgt->addresses.testA.go_to);
    }
  else
    {
    gg_append_statement(tgt->addresses.exit.go_to);
    }
  }

void
parser_alter( cbl_perform_tgt_t *tgt )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }
  cbl_label_t *altered    = tgt->from();
  cbl_label_t *proceed_to = tgt->to();

  struct cbl_proc_t *altered_proc = find_procedure(altered);
  struct cbl_proc_t *proceed_to_proc = find_procedure(proceed_to);

  gg_assign(  altered_proc->alter_location,
              proceed_to_proc->top.addr);
  }

void
parser_goto( cbl_refer_t value_ref, size_t narg, cbl_label_t * const labels[] )
  {
  // This is part of the Terrible Trio of parser_perform, parser_goto and
  // parser_enter_[procedure].  parser_goto has an easier time of it than
  // the other two, because it just has to jump from here to the entry point
  // of the paragraph [or section]
  Analyze();

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    for(size_t i=0; i<narg; i++)
      {
      SHOW_PARSE_TEXT(" ");
      SHOW_PARSE_TEXT(labels[i]->name);
      }
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    for(size_t i=0; i<narg; i++)
      {
      TRACE1_TEXT(labels[i]->name);
      TRACE1_TEXT(" ");
      }
    TRACE1_END
    }

  gcc_assert(narg >= 1);

  // This is a computed GOTO.  It might have only one element, which is
  // an ordinary GOTO without a DEPENDING ON clause.  We create that table
  // anyway, because in the case of an ALTER statement, we will be replacing
  // that sole element with the PROCEED TO element.

  // We need to create a static array of pointers to locations:
  static int comp_gotos = 1;
  char *psz = xasprintf("_comp_goto_%d", comp_gotos++);
  tree array_of_pointers_type = build_array_type_nelts(VOID_P, narg);
  tree array_of_pointers = gg_define_variable(array_of_pointers_type, psz, vs_static);
  free(psz);

  // We have the array.  Now we need to build the constructor for it
  tree constr = make_node(CONSTRUCTOR);
  TREE_TYPE(constr) = array_of_pointers_type;
  TREE_STATIC(constr)    = 1;
  TREE_CONSTANT(constr)  = 1;

  for(size_t i=0; i<narg; i++)
    {
    CHECK_LABEL(labels[i]);
    struct cbl_proc_t *procedure = find_procedure(labels[i]);
    CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr),
                            build_int_cst_type(SIZE_T, i),
                            procedure->top.addr );
    }
  DECL_INITIAL(array_of_pointers) = constr;

  // We need to pick up the value argument as an INT:
  tree value   = gg_define_int();

  if( value_ref.field )
    {
    get_binary_value( value,
                      NULL,
                      value_ref.field,
                      refer_offset_source(value_ref));
    // Convert it from one-based to zero-based:
    gg_decrement(value);
    // Check to see if the value is in the range 0...narg-1:
    IF( value, ge_op, integer_zero_node)
      {
      IF( value, lt_op, build_int_cst_type(INT, narg) )
        {
        // It is in the valid range, so we can do the goto:
        Analyzer.ExitMessage();
        gg_goto(gg_array_value(array_of_pointers, value));
        }
      ELSE
        {
        // Otherwise, just fall through
        }
        ENDIF
      }
    ELSE
      ENDIF
    }
  else
    {
    // This is a simple GOTO.  Because it is a simple GO TO, there is the
    // possibility that this paragraph was the target of an ALTER statement.
    IF( current_function->current_paragraph->alter_location, ne_op, null_pointer_node )
      {
      // Somebody did an ALTER statement before we got here
      gg_assign(current_function->void_star_temp, current_function->current_paragraph->alter_location);
      }
    ELSE
      {
      // This paragraph wasn't the target of an ALTER:
      gg_assign(current_function->void_star_temp, gg_array_value(array_of_pointers, 0));
      }
      ENDIF
    Analyzer.ExitMessage();
    gg_goto(current_function->void_star_temp);
    }
  return;
  }

void
parser_perform(cbl_label_t *label, bool suppress_nexting)
  {
  label->used = yylineno;
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_LABEL(" ", label)
    char ach[32];
    sprintf(ach, " label is at %p", (void*)label);
    SHOW_PARSE_TEXT(ach)
    sprintf(ach, " label->proc is %p", (void*)label->structs.proc);
    SHOW_PARSE_TEXT(ach)
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_LABEL("", label, "")
    TRACE1_END
    }

  CHECK_LABEL(label);

  struct cbl_proc_t *procedure = find_procedure(label);

  // We need to create the unnamed return address that we
  // will instantiate right after the goto:
  tree return_address_decl = build_decl(  UNKNOWN_LOCATION,
                                          LABEL_DECL,
                                          NULL_TREE,
                                          void_type_node);
  DECL_CONTEXT(return_address_decl) = current_function->function_decl;
  TREE_USED(return_address_decl) = 1;

  tree return_label_expr = build1(LABEL_EXPR,
                                  void_type_node,
                                  return_address_decl);
  tree return_addr = gg_get_address_of(return_address_decl);

//  cbl_parser_mod *parser_mod = new cbl_parser_mod;

  // Put the return address onto the pseudo-return stack
  pseudo_return_push(procedure, return_addr);

  // Create the code that will launch the paragraph
  // The following comment is, believe it or not, necessary.  The insertion
  // includes a line number insertion that's needed because when the goto/label
  // pairs were created, the locations of the goto instruction and the label
  // were not known.

  char *para_name     = nullptr;
  char *sect_name     = nullptr;
  const char *program_name  = current_function->our_unmangled_name;
  size_t deconflictor = symbol_label_id(label);

  char ach[256];
  if( label->type == LblParagraph )
    {
    cbl_label_t *section_label = cbl_label_of(symbol_at(label->parent));
    para_name = label->name;
    sect_name = section_label->name;
    sprintf(ach,
            "%s PERFORM %s of %s of %s (%ld)",
            ASM_COMMENT_START,
            para_name,
            sect_name,
            program_name,
            deconflictor);

    gg_insert_into_assembler(ach);
    }
  else
    {
    sect_name = label->name;
    sprintf(ach,
            "%s PERFORM %s of %s (%ld)",
            ASM_COMMENT_START,
            sect_name,
            program_name,
            deconflictor);
    gg_insert_into_assembler(ach);
    }

  if( !suppress_nexting )
    {
    sprintf(ach,
            "_proccall.%ld.%d:",
            symbol_label_id(label),
            call_counter++);
    gg_insert_into_assembler( ach );
    }

  // We do the indirect jump in order to prevent the compiler from complaining
  // in the case where we are performing a USE GLOBAL DECLARATIVE.  Without the
  // indirection, the compiler isn't able to handle the case where we are
  // jumping to a location in our parent program-id; it can't find a matching
  // local symbol, and crashes.
  gg_goto(procedure->top.addr);

  // And create the return address label:
  gg_append_statement(return_label_expr);
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_LABEL("back_from_performing ", label, "")
    TRACE1_END
    }
  }

void
parser_perform_times( cbl_label_t *proc_1, cbl_refer_t count )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_LABEL(" ", proc_1)
    SHOW_PARSE_REF(" ", count)
    SHOW_PARSE_TEXT(" TIMES")
    char ach[32];
    sprintf(ach, " proc_1 is at %p", (void*)proc_1);
    SHOW_PARSE_TEXT(ach)
    sprintf(ach, " proc_1->proc is %p", (void*)proc_1->structs.proc);
    SHOW_PARSE_TEXT(ach)
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }

  char ach[256];
  size_t our_pseudo_label = pseudo_label++;
  sprintf(ach,
          "_proccallb.%ld:",
          our_pseudo_label);
  gg_insert_into_assembler( ach );

  tree counter       = gg_define_variable(LONG);

  // Get the count:
  get_binary_value( counter,
                    NULL,
                    count.field,
                    refer_offset_source(count));

  // Make sure the initial count is valid:
  WHILE( counter, gt_op, gg_cast(LONG, integer_zero_node) )
    {
    static const bool suppress_nexting = true;
    parser_perform(proc_1, suppress_nexting);
    gg_decrement(counter);
    }
    WEND

  sprintf(ach,
          "_procretb.%ld:",
          our_pseudo_label);
  gg_insert_into_assembler(ach);
  }

static void
internal_perform_through( cbl_label_t *proc_1,
                          cbl_label_t *proc_2,
                          bool suppress_nexting )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_LABEL(" ", proc_1);
    char ach[32];
    sprintf(ach, " proc_1 is at %p", (void*)proc_1);
    SHOW_PARSE_TEXT(ach)
    sprintf(ach, " proc_1->proc is %p", (void*)proc_1->structs.proc);
    SHOW_PARSE_TEXT(ach)
    if( proc_2 )
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_LABEL("", proc_2);
      sprintf(ach, " proc_2 is at %p", (void*)proc_2);
      SHOW_PARSE_TEXT(ach)
      sprintf(ach, " proc_2->proc is %p", (void*)proc_2->structs.proc);
      SHOW_PARSE_TEXT(ach)
      }
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }

  CHECK_LABEL(proc_1);

  if(!proc_2)
    {
    parser_perform(proc_1, suppress_nexting);
    return;
    }

  CHECK_LABEL(proc_2);

  struct cbl_proc_t *proc1 = find_procedure(proc_1);
  struct cbl_proc_t *proc2 = find_procedure(proc_2);

  // We need to create the unnamed return address that we
  // will instantiate right after the goto:
  tree return_address_decl = build_decl(  UNKNOWN_LOCATION,
                                          LABEL_DECL,
                                          NULL_TREE,
                                          void_type_node);
  DECL_CONTEXT(return_address_decl) = current_function->function_decl;
  TREE_USED(return_address_decl) = 1;

  tree return_label_expr = build1(LABEL_EXPR,
                                  void_type_node,
                                  return_address_decl);
  tree return_addr = gg_get_address_of(return_address_decl);

  //cbl_parser_mod *parser_mod_proc1 = new cbl_parser_mod;
  //cbl_parser_mod *parser_mod_proc2 = new cbl_parser_mod;

  // Put the return address of the second procedure onto the stack:
  pseudo_return_push(proc2, return_addr);

  // Create the code that will launch the first procedure
  gg_insert_into_assembler("%s PERFORM %s THROUGH %s",
                        ASM_COMMENT_START, proc_1->name, proc_2->name);

  if( !suppress_nexting )
    {
    char ach[256];
    sprintf(ach,
            "_proccall.%ld.%d:",
            symbol_label_id(proc_2),
            call_counter++);
    gg_insert_into_assembler(ach);
    }

  gg_append_statement(proc1->top.go_to);

  // And create the return address label:
  gg_append_statement(return_label_expr);
  }

static void
internal_perform_through_times(   cbl_label_t *proc_1,
                                  cbl_label_t *proc_2,
                                  cbl_refer_t &count)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_LABEL(" ", proc_1);
    char ach[32];
    sprintf(ach, " proc_1 is at %p", (void*)proc_1);
    SHOW_PARSE_TEXT(ach)
    sprintf(ach, " proc_1->proc is %p", (void*)proc_1->structs.proc);
    SHOW_PARSE_TEXT(ach)
    if( proc_2 )
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_LABEL("", proc_2);
      sprintf(ach, " proc_2 is at %p", (void*)proc_2);
      SHOW_PARSE_TEXT(ach)
      sprintf(ach, " proc_2->proc is %p", (void*)proc_2->structs.proc);
      SHOW_PARSE_TEXT(ach)
      }
    SHOW_PARSE_REF(" ", count);
    SHOW_PARSE_TEXT(" TIMES");
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }

  size_t our_pseudo_label = pseudo_label++;

  char ach[256];
  sprintf(ach,
          "_proccallb.%ld:",
          our_pseudo_label);
  gg_insert_into_assembler( ach );

  tree counter       = gg_define_variable(LONG);
  get_binary_value( counter,
                    NULL,
                    count.field,
                    refer_offset_source(count));
  WHILE( counter, gt_op, gg_cast(LONG, integer_zero_node) )
    {
    internal_perform_through(proc_1, proc_2, true); // true means suppress_nexting
    gg_decrement(counter);
    }
    WEND

  sprintf(ach,
          "_procretb.%ld:",
          our_pseudo_label);
  gg_insert_into_assembler( ach );
  }

void
register_main_switch(const char *main_string)
  {
  char *mstr = xstrdup(main_string);
  char *p = strchr(mstr, ':');
  if( p )
    {
    *p = '\0';
    main_string = p+1;
    main_strings[mstr] = main_string;
    }
  else
    {
    main_strings[mstr] = "";
    }
  free(mstr);
  }

static int file_level = 0;

void
parser_first_statement( int lineno )
  {
  // In the event that this routine is the one that main() calls to get the
  // execution ball rolling, we want the GDB "start" function to be able
  // to set a temporary breakpoint at this location.  We get that rolling
  // here.

  char ach[256];

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    sprintf(ach, " lineno is %d, suppression is %d", lineno, suppress_cobol_entry_point);
    SHOW_PARSE_TEXT(ach)
    SHOW_PARSE_END
    }

  if(    strcmp(current_function->our_name, ach_cobol_entry_point) == 0
      && !suppress_cobol_entry_point )
    {
    sprintf(ach,
            "%s:%d",
            current_filename.back().c_str(),
            lineno);
    *ach_cobol_entry_point = '\0';
    create_cblc_string_variable("_cobol_entry_point", ach);

    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      char ach2[512];
      sprintf(ach2, "setting _cobol_entry_point to \"%s\"", ach);
      SHOW_PARSE_TEXT(ach2)
      SHOW_PARSE_END
      }
    }

  if( !suppress_cobol_entry_point )
    {
    char achentry[128];
    sprintf(ach,
            "%s:%d",
            current_filename.back().c_str(),
            lineno);

    sprintf(achentry, "_prog_entry_point_%s", current_function->our_name);
    create_cblc_string_variable(achentry, ach);
    }
  }

#define linemap_add(...)

void
parser_enter_file(const char *filename)
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    char *psz;
    psz = xasprintf(" entering level:%d %s", file_level+1, filename);
    SHOW_PARSE_TEXT(psz);
    free(psz);
    SHOW_PARSE_END
    }

  current_filename.push_back(filename);

  std::unordered_map<std::string, std::string>::const_iterator it
    = main_strings.find(filename);

  if( it != main_strings.end() )
    {
    // There was a -main switch for this file.
    this_module_has_main = true;
    next_program_is_main = true;

    const char *pname = it->second.c_str();
    if( pname && strlen(pname) )
      {
      main_entry_point = xstrdup(pname);
      }
    }

  // Let the linemap routine know we are working on a new file:
  linemap_add(line_table, LC_ENTER, 0, filename, 1);

  if( file_level == 0 )
    {
    // Build a translation_unit_decl:
    gg_build_translation_unit(filename);
    create_our_type_nodes();
    }

  file_level += 1;

  if( file_level == 1 )
    {
    // This table is used for "creating" the file-static named variables used in
    // the GENERIC we generate.

  // Establish our variable declarations for global variables in libgcobol:

#define SET_VAR_DECL(A, B, C) \
  A = gg_declare_variable(B, C, NULL_TREE, vs_external_reference)

    SET_VAR_DECL(var_decl_exception_code         , INT    , "__gg__exception_code");
    SET_VAR_DECL(var_decl_exception_handled      , INT    , "__gg__exception_handled");
    SET_VAR_DECL(var_decl_exception_file_number  , INT    , "__gg__exception_file_number");
    SET_VAR_DECL(var_decl_exception_file_status  , INT    , "__gg__exception_file_status");
    SET_VAR_DECL(var_decl_exception_file_name    , CHAR_P , "__gg__exception_file_name");
    SET_VAR_DECL(var_decl_exception_statement    , CHAR_P , "__gg__exception_statement");
    SET_VAR_DECL(var_decl_exception_source_file  , CHAR_P , "__gg__exception_source_file");
    SET_VAR_DECL(var_decl_exception_line_number  , INT    , "__gg__exception_line_number");
    SET_VAR_DECL(var_decl_exception_program_id   , CHAR_P , "__gg__exception_program_id");
    SET_VAR_DECL(var_decl_exception_section      , CHAR_P , "__gg__exception_section");
    SET_VAR_DECL(var_decl_exception_paragraph    , CHAR_P , "__gg__exception_paragraph");

    SET_VAR_DECL(var_decl_default_compute_error  , INT    , "__gg__default_compute_error");
    SET_VAR_DECL(var_decl_rdigits                , INT    , "__gg__rdigits");
    SET_VAR_DECL(var_decl_odo_violation          , INT    , "__gg__odo_violation");
    SET_VAR_DECL(var_decl_unique_prog_id         , SIZE_T , "__gg__unique_prog_id");

    SET_VAR_DECL(var_decl_entry_location         , VOID_P , "__gg__entry_pointer");
    SET_VAR_DECL(var_decl_exit_address           , VOID_P , "__gg__exit_address");

    SET_VAR_DECL(var_decl_call_parameter_signature , CHAR_P   , "__gg__call_parameter_signature");
    SET_VAR_DECL(var_decl_call_parameter_count     , INT      , "__gg__call_parameter_count");
    SET_VAR_DECL(var_decl_call_parameter_lengths   , build_array_type(SIZE_T, NULL),
                                                            "__gg__call_parameter_lengths");
    SET_VAR_DECL(var_decl_return_code             , SHORT      , "__gg__data_return_code");

    SET_VAR_DECL(var_decl_arithmetic_rounds_size  , SIZE_T , "__gg__arithmetic_rounds_size");
    SET_VAR_DECL(var_decl_arithmetic_rounds       , INT_P  , "__gg__arithmetic_rounds");
    SET_VAR_DECL(var_decl_fourplet_flags_size     , SIZE_T , "__gg__fourplet_flags_size");
    SET_VAR_DECL(var_decl_fourplet_flags          , INT_P  , "__gg__fourplet_flags");

    SET_VAR_DECL(var_decl_treeplet_1f             , cblc_field_pp_type_node , "__gg__treeplet_1f"     );
    SET_VAR_DECL(var_decl_treeplet_1o             , SIZE_T_P                , "__gg__treeplet_1o"     );
    SET_VAR_DECL(var_decl_treeplet_1s             , SIZE_T_P                , "__gg__treeplet_1s"     );
    SET_VAR_DECL(var_decl_treeplet_2f             , cblc_field_pp_type_node , "__gg__treeplet_2f"     );
    SET_VAR_DECL(var_decl_treeplet_2o             , SIZE_T_P                , "__gg__treeplet_2o"     );
    SET_VAR_DECL(var_decl_treeplet_2s             , SIZE_T_P                , "__gg__treeplet_2s"     );
    SET_VAR_DECL(var_decl_treeplet_3f             , cblc_field_pp_type_node , "__gg__treeplet_3f"     );
    SET_VAR_DECL(var_decl_treeplet_3o             , SIZE_T_P                , "__gg__treeplet_3o"     );
    SET_VAR_DECL(var_decl_treeplet_3s             , SIZE_T_P                , "__gg__treeplet_3s"     );
    SET_VAR_DECL(var_decl_treeplet_4f             , cblc_field_pp_type_node , "__gg__treeplet_4f"     );
    SET_VAR_DECL(var_decl_treeplet_4o             , SIZE_T_P                , "__gg__treeplet_4o"     );
    SET_VAR_DECL(var_decl_treeplet_4s             , SIZE_T_P                , "__gg__treeplet_4s"     );
    SET_VAR_DECL(var_decl_nop                     , INT                     , "__gg__nop"             );
    SET_VAR_DECL(var_decl_main_called             , INT                     , "__gg__main_called"     );
    }
  }

void
parser_leave_file()
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    char ach[256];
    sprintf(ach, "leaving level:%d %s", file_level, current_filename.back().c_str());
    SHOW_PARSE_TEXT(ach)
    SHOW_PARSE_END
    }
  if( file_level > 0)
    {
    linemap_add(line_table, LC_LEAVE, false, NULL, 0);
    }
  file_level -= 1;
  current_filename.pop_back();
  }

void
enter_program_common(const char *funcname, const char *funcname_)
  {
  // We arrive here when processing a PROGRAM-ID.

  // At this point, we don't know how many formal parameters there are going
  // to be.

  // We are going to create a function returning a 64-bit value, but it'll
  // have no parameters.  We'll chain the parameters on in parser_division(),
  // when we process PROCEDURE DIVISION USING...

  gg_define_function_with_no_parameters( COBOL_FUNCTION_RETURN_TYPE,
                                         funcname,
                                         funcname_);

  current_function->first_time_through =
    gg_define_variable(INT,
                        "_first_time_through",
                        vs_static,
                        integer_one_node);

  gg_create_goto_pair(&current_function->skip_init_goto,
                      &current_function->skip_init_label);

  IF( current_function->first_time_through, eq_op, integer_zero_node )
    gg_append_statement(current_function->skip_init_goto);
  ELSE
    ENDIF

  gg_assign(current_function->first_time_through, integer_zero_node);

  // Establish variables that are function-wide in scope:
  current_function->void_star_temp = gg_define_void_star("_void_star_temp");

  current_function->perform_exit_address
    = gg_define_void_star("_perform_exit_address");

  // Make sure the following are null, because when we create the unnamed
  // default section, parser_enter_section will attempt to close them out. And
  // it's possible on the first go-through that they have garbage values.

  current_function->current_section = NULL;
  current_function->current_paragraph = NULL;

  current_function->is_truly_nested = false;

  // Text conversion must be initialized before the code generated by
  // parser_symbol_add runs.

  // The text_conversion_override exists both in the library and in the compiler

  __gg__set_internal_codeset(internal_codeset_is_ebcdic());
  gg_call(VOID,
          "__gg__set_internal_codeset",
          internal_codeset_is_ebcdic()
          ? integer_one_node : integer_zero_node,
          NULL_TREE);

  __gg__text_conversion_override(td_default_e, cs_default_e);
  gg_call(VOID,
          "__gg__text_conversion_override",
          build_int_cst_type(INT, td_default_e),
          build_int_cst_type(INT, cs_default_e),
          NULL_TREE);

  gg_call(VOID,
          "__gg__codeset_figurative_constants",
          NULL_TREE);

  static int counter=1;
  char ach[32];

  sprintf(ach, "_cf_fds_%d", counter);
  current_function->first_declarative_section
                  = gg_define_variable(CHAR_P,
                                       ach,
                                       vs_static,
                                       null_pointer_node);
  sprintf(ach, "_cf_cbmc_%d", counter);
  current_function->called_by_main_counter = gg_define_variable(INT,
                                                          ach,
                                                          vs_static,
                                                          integer_zero_node);
  counter += 1;

  // Initialize the TRACE logic, which has to be done before the first TRACE1
  // invocation, but after there is a function to lay down GIMPLE code in.

  // That is to say: Here.  Multiple invocations of trace1_init are harmless.
  trace1_init();
  }

/*  Creates a function for program-id 'funcname_'.  Returns 1 when funcname_
    is "main" and the -main compiler switch is active for this moudle */

void
parser_enter_program( const char *funcname_,
                      bool is_function,  // True for user-defined-function
                      int *pretval)
  {
  *pretval = 0;

  // The first thing we have to do is mangle this name.  This is safe even
  // though the end result will be mangled again, because the mangler doesn't
  // change a mangled name.
  char *mangled_name = cobol_name_mangler(funcname_);

  size_t parent_index = current_program_index();
  char funcname[128];
  if( parent_index )
    {
    // This is a nested function.  Tack on the parent_index to the end of it.
    sprintf(funcname, "%s.%ld", mangled_name, parent_index);
    }
  else
    {
    // This is a top-level function; just use the straight mangled name
    strcpy(funcname, mangled_name);
    }
  free(mangled_name);

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" ")
    SHOW_PARSE_TEXT(funcname)
    SHOW_PARSE_END
    }

  if( !is_function && !parent_index )
    {
    // This is a top_level program, and not a function
    if( next_program_is_main )
      {
      next_program_is_main = false;
      if(main_entry_point)
        {
        build_main_that_calls_something(main_entry_point);
        free(main_entry_point);
        main_entry_point = NULL;
        }
      else
        {
        build_main_that_calls_something(funcname);
        }
      }
    }

  // Call this after build_main_that_calls_something, because it manipulates
  // the current line number to DEFAULT_LINE_NUMBER.  We have to manipulate it
  // back afterward.
  gg_set_current_line_number(CURRENT_LINE_NUMBER);

  if( strcmp(funcname_, "main") == 0 && this_module_has_main )
    {
    // setting 'retval' to 1 let's the caller know that we are being told
    // both to synthesize a main() entry point to duplicate GCC's default
    // behavior, and to create an explicit entry point named "main".  This will
    // eventually result in a link error (because of the duplicated entry
    // points.  The return value serves as an alert; it's up to the caller to
    // decide what to do.
    *pretval = 1;
    }

  if( strcmp(funcname, "dubner") == 0)
    {
    // This should be enabled by an environment variable.
    // But for now I am being cutesy
    hijack_for_development(funcname);
    return;
    }

  enter_program_common(funcname, funcname_);
  current_function->is_function = is_function;

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("entered program \"")
    TRACE1_TEXT(funcname)
    TRACE1_TEXT("\"")
    TRACE1_END
    }
  }

void
parser_end_program(const char *prog_name  )
  {
  if( gg_trans_unit.function_stack.size() )
    {
    // The body has been created by various parser calls.  It's time
    // to wrap this sucker up!

    // Ending the program ends the current paragraph and section:
    leave_paragraph_internal();
    leave_section_internal();
    }

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    TRACE1_TEXT_ABC("\"", prog_name, "\"")
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT_ABC("\"", prog_name, "\"")
    TRACE1_END
    }

  if( gg_trans_unit.function_stack.size() )
    {
    // The body has been created by various parser calls.  It's time
    // to wrap this sucker up!

    // Put in a harmless return in case there was no EXIT PROGRAM statement.
    // It's harmless because if it isn't needed, a return was already
    // executed, and this generated code will never be executed
    parser_exit( cbl_refer_t() );

    // Tell the GCC compiler to do the GIMPLIFY thing.
    gg_finalize_function();
    }
  }

static void
remove_p_from_picture(char *picture)
  {
  // At this point, attr has the scaled_e flag, and rdigits tells us
  // which way to scale.  So, the P characters in picture are now
  // a liability.

  char *rabbit = picture;
  char *fox = picture;

  for(;;)
    {
    char ch = *rabbit++;
    if( ch == '\0' )
      {
      break;
      }
    if( ch == 'P' || ch == 'p' )
      {
      if( *rabbit == '(' )
        {
        while( *rabbit != ')' )
          {
          rabbit += 1;
          }
        rabbit += 1;
        // rabbit now points to one past the closing parenthesis
        }
      size_t to_move = strlen(rabbit);
      memmove(fox, rabbit, to_move+1);  // +1 snags the '\0'
      rabbit = fox;
      }
    else
      {
      fox += 1;
      }
    }
  }

static tree vti_array;
static tree vti_constructor;
static int  vti_list_size;
static int  vti_next_variable;

void
parser_init_list_size(int count_of_variables)
  {
  if( mode_syntax_only() ) return;

  vti_list_size = count_of_variables;
  char ach[48];
  sprintf(ach,
          "..variables_to_init_%ld",
          current_function->our_symbol_table_index);
  tree array_of_variables_type = build_array_type_nelts(VOID_P,
                                                        count_of_variables+1);
  vti_array = gg_define_variable( array_of_variables_type,
                                  ach,
                                  vs_file_static);
  vti_constructor                 = make_node(CONSTRUCTOR);
  TREE_TYPE(vti_constructor)      = array_of_variables_type;
  TREE_STATIC(vti_constructor)    = 1;
  TREE_CONSTANT(vti_constructor)  = 1;
  vti_next_variable = 0;
  }

void
parser_init_list_element(cbl_field_t *field)
  {
  if( mode_syntax_only() ) return;

  gcc_assert(vti_next_variable < vti_list_size);
  CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(vti_constructor),
                          build_int_cst_type(SIZE_T, vti_next_variable++),
                          gg_get_address_of(field->var_decl_node) );
  if( vti_next_variable == vti_list_size)
    {
    CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(vti_constructor),
                            build_int_cst_type(SIZE_T, vti_next_variable++),
                            null_pointer_node );
    DECL_INITIAL(vti_array) = vti_constructor;
    }
  }

void
parser_init_list()
  {
  if( mode_syntax_only() ) return;

  char ach[48];
  sprintf(ach,
          "..variables_to_init_%ld",
          current_function->our_symbol_table_index);
  tree array = gg_trans_unit_var_decl(ach);
  gg_call(VOID,
          "__gg__variables_to_init",
          gg_get_address_of(array),
          wsclear() ? gg_string_literal(wsclear()) : null_pointer_node,
          NULL_TREE);
  }

static void
psa_FldLiteralN(struct cbl_field_t *field )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD(" ", field)
    SHOW_PARSE_END
    }
  // We are constructing a completely static constant structure, based on the
  // text string in .initial

  FIXED_WIDE_INT(128) value = 0;

  do
    {
    // This is a false do{}while, to isolate the variables:

    // We need to convert data.initial to an FIXED_WIDE_INT(128) value
    char *p = const_cast<char *>(field->data.initial);
    int sign = 1;
    if( *p == '-' )
      {
      field->attr |= signable_e;
      sign = -1;
      p += 1;
      }
    else if( *p == '+' )
      {
      // We set it signable so that the instruction DISPLAY +1
      // actually outputs "+1"
      field->attr |= signable_e;
      p += 1;
      }

    //  We need to be able to handle
    //  123
    //  123.456
    //  123E<exp>
    //  123.456E<exp>
    //  where <exp> can be N, +N and -N
    //
    //  Oh, yeah, and we're talking handling up to 32 digits, or more, so using
    //  library routines is off the table.

    int digits = 0;
    int rdigits = 0;
    int rdigit_delta = 0;
    int exponent = 0;

    char *exp = strchr(p, 'E');
    if( !exp )
      {
      exp = strchr(p, 'e');
      }
    if(exp)
      {
      exponent = atoi(exp+1);
      }

    // We can now calculate the value, and the number of digits and rdigits.

    // We count up leading zeroes as part of the attr->digits calculation.
    // It turns out that certain comparisons need to know the number of digits,
    // because "IF "2" EQUAL 002" is false, while "IF "2" EQUAL 2" is true.  So,
    // we need to count up leading zeroes.

    for(;;)
      {
      char ch = *p++;
      if( ch == symbol_decimal_point() )
        {
        rdigit_delta = 1;
        continue;
        }
      if( ch < '0' || ch > '9' )
        {
        break;
        }
      digits += 1;
      rdigits += rdigit_delta;
      value *= 10;
      value += ch - '0';
      }

    if( exponent < 0 )
      {
      rdigits += -exponent;
      }
    else
      {
      while(exponent--)
        {
        if(rdigits)
          {
          rdigits -= 1;
          }
        else
          {
          digits += 1;
          value *= 10;
          }
        }
      }

    if(digits < rdigits)
      {
      digits = rdigits;
      }
    field->data.digits = digits;
    field->data.rdigits = rdigits;

    // We now need to calculate the capacity.

    unsigned int min_prec = wi::min_precision(value, UNSIGNED);
    int capacity;
    if( min_prec > 64 )
      {
      // Bytes 15 through 8 are non-zero
      capacity = 16;
      }
    else if( min_prec > 32 )
      {
      // Bytes 7 through 4 are non-zero
      capacity = 8;
      }
    else if( min_prec > 16 )
      {
      // Bytes 3 and 2
      capacity = 4;
      }
    else if( min_prec > 8 )
      {
      // Byte 1 is non-zero
      capacity = 2;
      }
    else
      {
      // The value is zero through 0xFF
      capacity = 1;
      }

    value *= sign;

    // One last adjustment.  The number is signable, so the binary value
    // is going to be treated as twos complement.  That means that the highest
    // bit has to be 1 for negative signable numbers, and 0 for positive.  If
    // necessary, adjust capacity up by one byte so that the variable fits:

    if( capacity < 16 && (field->attr & signable_e) )
      {
      FIXED_WIDE_INT(128) mask
        = wi::set_bit_in_zero<FIXED_WIDE_INT(128)>(capacity * 8 - 1);
      if( wi::neg_p (value) && (value & mask) == 0 )
        {
        capacity *= 2;
        }
      else if( !wi::neg_p (value) && (value & mask) != 0 )
        {
        capacity *= 2;
        }
      }
    field->data.capacity = capacity;

    }while(0);

  char base_name[257];
  char id_string[32] = "";

  static size_t our_index = 0;

  sprintf(id_string, ".%ld", ++our_index);
  strcpy(base_name, field->name);
  strcat(base_name, id_string);

  tree var_type;

  // The value is 1, 2, 4, 8 or 16 bytes, so an ordinary constructor can be
  // used.
  var_type = tree_type_from_size( field->data.capacity,
                                  field->attr & signable_e);
  tree new_var_decl = gg_define_variable( var_type,
                                          base_name,
                                          vs_static);
  DECL_INITIAL(new_var_decl) = wide_int_to_tree(var_type, value);
  field->data_decl_node = new_var_decl;
  }

static void
psa_FldBlob(struct cbl_field_t *var )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD(" ", var)
    SHOW_PARSE_END
    }

  // We are constructing a completely static constant structure.  We know the
  // capacity.  We'll create it from the data.initial.  The var_decl_node will
  // be a pointer to the data

  char base_name[257];
  char id_string[32] = "";

  static size_t our_index = 0;

  sprintf(id_string, ".%ld", ++our_index);
  strcpy(base_name, var->name);
  strcat(base_name, id_string);

  // Build the constructor for the array of bytes

  tree array_type                  = build_array_type_nelts(UCHAR, var->data.capacity);
  tree array_constructor           = make_node(CONSTRUCTOR);
  TREE_TYPE(array_constructor)     = array_type;
  TREE_STATIC(array_constructor)   = 1;
  TREE_CONSTANT(array_constructor) = 1;

  for(size_t i=0; i<var->data.capacity; i++)
    {
    CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(array_constructor),
                            build_int_cst_type(INT, i),
                            build_int_cst_type(UCHAR, var->data.initial[i]));
    }

  // The array constructor is ready to be used
  tree var_decl_node = gg_define_variable( array_type,
                                          base_name,
                                          vs_static);
  DECL_INITIAL(var_decl_node) = array_constructor;
  var->var_decl_node = gg_get_address_of(var_decl_node);
  }

void
parser_accept(  struct cbl_refer_t refer,
                enum special_name_t special_e )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_REF(" ", refer);
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }

  /*
  enum special_name_t
      {
        SYSIN_e,
        SYSIPT_e,
        SYSOUT_e,
        SYSLIST_e,
        SYSLST_e,
        SYSPUNCH_e,
        SYSPCH_e,
        CONSOLE_e,
        C01_e, C02_e, C03_e, C04_e, C05_e, C06_e,
        C07_e, C08_e, C09_e, C10_e, C11_e, C12_e,
        CSP_e,
        S01_e, S02_e, S03_e, S04_e, S05_e,
        AFP_5A_e,
  };
  */

  // The ISO spec describes the valid special names for ACCEPT as implementation
  // dependent.  We are following IBM's lead.

  tree environment = build_int_cst_type(INT, special_e);

  switch( special_e )
    {
    case CONSOLE_e:
    case SYSIPT_e:
    case SYSIN_e:
      break;
    default:
      dbgmsg("%s(): We don't know what to do with special_name_t %d,", __func__, special_e);
      dbgmsg("%s(): so we are ignoring it.", __func__);
      yywarn("unrecognized SPECIAL NAME ignored");
      return;
      break;
    }

  gg_call(VOID,
          "__gg__accept",
          environment,
          gg_get_address_of(refer.field->var_decl_node),
          refer_offset_dest(refer),
          refer_size_dest(refer),
          NULL_TREE);
  }

// TODO: update documentation.
void
parser_accept_exception( cbl_label_t *accept_label )
  {
  // We can't use Analyze() on this one, because the exit ends up being laid
  // down before the enter when the goto logic gets untangled by the compiler.

  // We are entering either SIZE ERROR or NOT SIZE ERROR code
  RETURN_IF_PARSE_ONLY;
  set_up_on_exception_label(accept_label);

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" Laying down GOTO OVER")
    SHOW_PARSE_LABEL(" ", accept_label)
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT("Laying down LABEL INTO:")
    SHOW_PARSE_LABEL(" ", accept_label)
    SHOW_PARSE_END
    }

  // Jump over the [NOT] ON EXCEPTION code that is about to be laid down
  gg_append_statement( accept_label->structs.arith_error->over.go_to );
  // Create the label that allows the following code to be executed at
  // when an ERROR, or NOT ERROR, has been determined to have taken place:
  gg_append_statement( accept_label->structs.arith_error->into.label );
  }

void
parser_accept_exception_end( cbl_label_t *accept_label )
  {
  // We can't use Analyze() on this one, because the exit ends up being laid
  // down before the enter when the goto logic gets untangled by the compiler.

  // We have reached the end of the ERROR, or NOT ERROR, code.

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" Laying down GOTO BOTTOM")
    SHOW_PARSE_LABEL(" ", accept_label)
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT("Laying down LABEL OVER:")
    SHOW_PARSE_LABEL(" ", accept_label)
    SHOW_PARSE_END
    }

  // Jump to the end of the arithmetic code:
  gg_append_statement( accept_label->structs.arith_error->bottom.go_to );
  // Lay down the label that allows the ERROR/NOT ERROR instructions
  // to exist in a lacuna that doesn't get executed unless somebody jumps
  // to it:
  gg_append_statement( accept_label->structs.arith_error->over.label );
  }

void
parser_accept_command_line( cbl_refer_t tgt,
                            cbl_refer_t source,
                            cbl_label_t *error,
                            cbl_label_t *not_error )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( error )
      {
      SHOW_PARSE_LABEL(" error ", error)
      }
    if( not_error )
      {
      SHOW_PARSE_LABEL(" not_error ", not_error)
      }
    SHOW_PARSE_END
    }

  static tree erf = gg_define_variable(INT, "..pac_erf", vs_file_static);

  if( !source.field )
    {
    // The whole command-line is wanted
    gg_assign(erf,
              gg_call_expr( INT,
                            "__gg__get_command_line",
                            gg_get_address_of(tgt.field->var_decl_node),
                            refer_offset_dest(tgt),
                            refer_size_dest(tgt),
                            NULL_TREE));
    if( error )
      {
      // There is an ON EXCEPTION phrase:
      IF( erf, ne_op, integer_zero_node )
        {
        SHOW_PARSE
          {
          SHOW_PARSE_INDENT
          SHOW_PARSE_TEXT("Laying down GOTO     error->INTO for_command_line")
          SHOW_PARSE_LABEL(" ", error)
          }
        gg_append_statement( error->structs.arith_error->into.go_to );
        }
      ELSE
        {
        }
        ENDIF
      }
    if( not_error )
      {
      // There is an NOT ON EXCEPTION phrase:
      IF( erf, eq_op, integer_zero_node )
        {
        SHOW_PARSE
          {
          SHOW_PARSE_INDENT
          SHOW_PARSE_TEXT("Laying down GOTO not_error->INTO for command_line")
          SHOW_PARSE_LABEL(" ", not_error)
          }
        gg_append_statement( not_error->structs.arith_error->into.go_to );
        }
      ELSE
        {
        }
        ENDIF
      }
    }
  else
    {
    // A particular parameter has been requested:
    gg_assign(erf,
              gg_call_expr(  INT,
                            "__gg__get_argv",
                            gg_get_address_of(tgt.field->var_decl_node),
                            refer_offset_dest(tgt),
                            refer_size_dest(tgt),
                            gg_get_address_of(source.field->var_decl_node),
                            refer_offset_dest(source),
                            refer_size_dest(source),
                            NULL_TREE));
    if( error )
      {
      // There is an ON EXCEPTION phrase:
      IF( erf, ne_op, integer_zero_node )
        {
        SHOW_PARSE
          {
          SHOW_PARSE_INDENT
          SHOW_PARSE_TEXT("Laying down GOTO     error->INTO for_argv")
          SHOW_PARSE_LABEL(" ", error)
          }
        gg_append_statement( error->structs.arith_error->into.go_to );
        }
      ELSE
        {
        }
        ENDIF
      }
    if( not_error )
      {
      // There is an NOT ON EXCEPTION phrase:
      IF( erf, eq_op, integer_zero_node )
        {
        SHOW_PARSE
          {
          SHOW_PARSE_INDENT
          SHOW_PARSE_TEXT("Laying down GOTO not_error->INTO for_argv")
          SHOW_PARSE_LABEL(" ", not_error)
          }
        gg_append_statement( not_error->structs.arith_error->into.go_to );
        }
      ELSE
        {
        }
        ENDIF
      }
    }
  if( error )
    {
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT("Laying down LABEL     error->bottom")
      SHOW_PARSE_LABEL(" ", error)
      }
    gg_append_statement( error->structs.arith_error->bottom.label );
    }
  if( not_error )
    {
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT("Laying down LABEL not_error->bottom")
      SHOW_PARSE_LABEL(" ", not_error)
      SHOW_PARSE_END
      }
    gg_append_statement( not_error->structs.arith_error->bottom.label );
    }
  }

void
parser_accept_command_line_count( cbl_refer_t tgt )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }
  gg_call(  VOID,
            "__gg__get_argc",
            gg_get_address_of(tgt.field->var_decl_node),
            refer_offset_dest(tgt),
            refer_size_dest(tgt),
            NULL_TREE);
  }

void
parser_accept_envar(struct cbl_refer_t tgt,
                    struct cbl_refer_t envar,
                    cbl_label_t *error,
                    cbl_label_t *not_error )
  {
  Analyze();

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( error )
      {
      SHOW_PARSE_LABEL(" error ", error)
      }
    if( not_error )
      {
      SHOW_PARSE_LABEL(" not_error ", not_error)
      }
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }

  static tree erf = gg_define_variable(INT, "..pae_erf", vs_file_static);

  gg_assign(erf,
            gg_call_expr( INT,
                          "__gg__accept_envar",
                          gg_get_address_of(tgt.field->var_decl_node),
                          refer_offset_dest(tgt),
                          refer_size_dest(tgt),
                          gg_get_address_of(envar.field->var_decl_node),
                          refer_offset_source(envar),
                          refer_size_source(envar),
                          NULL_TREE));
  if( error )
    {
    // There is an ON EXCEPTION phrase:
    IF( erf, ne_op, integer_zero_node )
      {
      gg_append_statement( error->structs.arith_error->into.go_to );
      }
    ELSE
      {
      }
      ENDIF
    }
  if( not_error )
    {
    // There is an NOT ON EXCEPTION phrase:
    IF( erf, eq_op, integer_zero_node )
      {
      gg_append_statement( not_error->structs.arith_error->into.go_to );
      }
    ELSE
      {
      }
      ENDIF
    }
  if( error )
    {
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT("Laying down LABEL     error->bottom")
      SHOW_PARSE_LABEL(" ", error)
      }
    gg_append_statement( error->structs.arith_error->bottom.label );
    }
  if( not_error )
    {
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT("Laying down LABEL not_error->bottom")
      SHOW_PARSE_LABEL(" ", not_error)
      SHOW_PARSE_END
      }
    gg_append_statement( not_error->structs.arith_error->bottom.label );
    }
  }

void
parser_set_envar( struct cbl_refer_t name, struct cbl_refer_t value )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }

  // Set name to value using setenv(3)
  gg_call(BOOL,
          "__gg__set_envar",
          gg_get_address_of(name.field->var_decl_node),
          refer_offset_source(name),
          refer_size_source(name),
          gg_get_address_of(value.field->var_decl_node),
          refer_offset_source(value),
          refer_size_source(value),
          NULL_TREE);
  }

void
parser_accept_date_yymmdd( struct cbl_field_t *target )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }

  CHECK_FIELD(target);

  tree pointer = gg_define_char_star();
  gg_assign(pointer, gg_call_expr(CHAR_P,
                                  "__gg__get_date_yymmdd",
                                  NULL_TREE));
  gg_default_qualification(target);
  move_tree_to_field( target,
                      pointer);

  gg_free(pointer);

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_FIELD("", target, "")
    TRACE1_END
    }
  }

void
parser_accept_date_yyyymmdd( struct cbl_field_t *target )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }

  CHECK_FIELD(target);

  tree pointer = gg_define_char_star();
  gg_assign(pointer, gg_call_expr(CHAR_P,
                                  "__gg__get_date_yyyymmdd",
                                  NULL_TREE));
  gg_default_qualification(target);
  move_tree_to_field( target,
                      pointer);

  gg_free(pointer);

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_FIELD("", target, "")
    TRACE1_END
    }
  }

void
parser_accept_date_yyddd( struct cbl_field_t *target )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }

  CHECK_FIELD(target);

  tree pointer = gg_define_char_star();
  gg_assign(pointer, gg_call_expr(CHAR_P,
                                  "__gg__get_date_yyddd",
                                  NULL_TREE));
  gg_default_qualification(target);
  move_tree_to_field( target,
                      pointer);

  gg_free(pointer);

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_FIELD("", target,"");
    TRACE1_END
    }
  }

void
parser_accept_date_yyyyddd( struct cbl_field_t *target )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }

  CHECK_FIELD(target);

  tree pointer = gg_define_char_star();
  gg_assign(pointer, gg_call_expr(CHAR_P,
                                  "__gg__get_yyyyddd",
                                  NULL_TREE));
  gg_default_qualification(target);
  move_tree_to_field( target,
                      pointer);

  gg_free(pointer);

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_FIELD("", target, "")
    TRACE1_END
    }
  }

void
parser_accept_date_dow( struct cbl_field_t *target )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }

  CHECK_FIELD(target);

  tree pointer = gg_define_char_star();
  gg_assign(pointer, gg_call_expr(CHAR_P,
                                  "__gg__get_date_dow",
                                  NULL_TREE));
  gg_default_qualification(target);
  move_tree_to_field( target,
                      pointer);

  gg_free(pointer);

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_FIELD("", target, "")
    TRACE1_END
    }
  }

void
parser_accept_date_hhmmssff( struct cbl_field_t *target )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }

  CHECK_FIELD(target);

  tree pointer = gg_define_char_star();
  gg_assign(pointer, gg_call_expr(CHAR_P,
                                  "__gg__get_date_hhmmssff",
                                  NULL_TREE));
  gg_default_qualification(target);
  move_tree_to_field( target,
                      pointer);

  gg_free(pointer);

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_FIELD("", target, "")
    TRACE1_END
    }
  }

/*
 * If the encoding is anything but custom, the enumerated type
 * cbl_encoding_t suffices to describe it.  At least for now, the rest
 * of cbl_alphabet_t in those cases is unused.
 *
 * To get the symbol index: symbol_index(symbol_elem_of(&alphabet))
 *
 * The parameter is always a reference to an element in the symbol table.
 */
void
parser_alphabet( cbl_alphabet_t& alphabet )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    fprintf(stderr, "%s\n", alphabet.name);
    switch(alphabet.encoding)
      {
      case ASCII_e:
        fprintf(stderr, "ASCII\n");
        break;
      case iso646_e:
        fprintf(stderr, "ISO646\n");
        break;
      case EBCDIC_e:
        fprintf(stderr, "EBCDIC\n");
        break;
      case custom_encoding_e:
        fprintf(stderr, "%s\n", alphabet.name);
        break;
      }
    SHOW_PARSE_END
    }

  size_t alphabet_index = symbol_index(symbol_elem_of(&alphabet));

  switch(alphabet.encoding)
    {
    case ASCII_e:
    case iso646_e:
    case EBCDIC_e:
      break;

    case custom_encoding_e:
      {
      unsigned char ach[256];

      tree table_type = build_array_type_nelts(UCHAR, 256);
      tree table256   = gg_define_variable(table_type);
      for( int i=0; i<256; i++ )
        {
        // character i has the ordinal alphabet[i]
        unsigned char ch = ascii_to_internal(i);

        ach[ch] = (alphabet.alphabet[i]);
        gg_assign(  gg_array_value(table256, ch),
                    build_int_cst_type(UCHAR, (alphabet.alphabet[i])) );
        }
      __gg__alphabet_create(alphabet.encoding,
                            alphabet_index,
                            ach,
                            alphabet.low_index,
                            alphabet.high_index);
      gg_call(VOID,
              "__gg__alphabet_create",
              build_int_cst_type(INT, alphabet.encoding),
              build_int_cst_type(SIZE_T, alphabet_index),
              gg_get_address_of(table256),
              build_int_cst_type(INT, alphabet.low_index),
              build_int_cst_type(INT, alphabet.high_index),
              NULL_TREE );
      break;
      }
    }
  }

void
parser_alphabet_use( cbl_alphabet_t& alphabet )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    switch(alphabet.encoding)
      {
      case ASCII_e:
        fprintf(stderr, "ASCII\n");
        break;
      case iso646_e:
        fprintf(stderr, "ISO646\n");
        break;
      case EBCDIC_e:
        fprintf(stderr, "EBCDIC\n");
        break;
      case custom_encoding_e:
        fprintf(stderr, "%s\n", alphabet.name);
        break;
      }
    SHOW_PARSE_END
    }

  size_t alphabet_index = symbol_index(symbol_elem_of(&alphabet));

  switch(alphabet.encoding)
    {
    case ASCII_e:
    case iso646_e:
    case EBCDIC_e:
      __gg__low_value_character  = DEGENERATE_LOW_VALUE;
      __gg__high_value_character = DEGENERATE_HIGH_VALUE;
      gg_call(VOID,
              "__gg__alphabet_use",
              build_int_cst_type(INT, alphabet.encoding),
              null_pointer_node,
              NULL_TREE);
      break;

    case custom_encoding_e:
      std::unordered_map<size_t, alphabet_state>::const_iterator it =
        __gg__alphabet_states.find(alphabet_index);

      assert( it != __gg__alphabet_states.end());
      __gg__low_value_character  = it->second.low_char;
      __gg__high_value_character = it->second.high_char;

      gg_call(VOID,
              "__gg__alphabet_use",
              build_int_cst_type(INT, alphabet.encoding),
              build_int_cst_type(SIZE_T, alphabet_index),
              NULL_TREE);
      break;
    }
  }

void
parser_display_literal(const char *literal, bool advance)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" \"");
    SHOW_PARSE_TEXT(literal)
    SHOW_PARSE_TEXT("\"");
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("About to DISPLAY a literal:")
    TRACE1_END
    }

  tree file_descriptor = integer_one_node; // Just stdout, for now
  gg_write(  file_descriptor,
             gg_string_literal(literal),
             build_int_cst_type(integer_type_node,(int)strlen(literal)) );

  if( advance )
    {
    gg_write(  file_descriptor,
               gg_string_literal("\n"),
               integer_one_node);
    }
  cursor_at_sol = advance;
  }

void
parser_display_internal(tree file_descriptor,
                        cbl_refer_t refer,
                        bool advance)
  {
  Analyze();
  if( refer.field->type == FldConditional )
    {
    TRACE1
      {
      gg_create_true_false_statement_lists(refer.field->var_decl_node);
        gg_fprintf(file_descriptor, 0, "TRUE");
      ELSE
        gg_fprintf(file_descriptor, 0, "FALSE");
      ENDIF
      }
    }
  else if( refer.field->type == FldLiteralA )
    {
    gg_call(VOID,
            "__gg__display_string",
            file_descriptor,
            build_string_literal(refer.field->data.capacity,
                                 refer.field->data.initial),
            build_int_cst_type(SIZE_T, refer.field->data.capacity),
            advance ? integer_one_node : integer_zero_node,
            NULL_TREE );
    cursor_at_sol = advance;
    }
  else if( refer.field->type == FldLiteralN )
    {
    // The parser found the string of digits from the source code and converted
    // it to a 128-bit binary floating point number.

    // The bad news is that something like 555.55 can't be expressed exactly;
    // internally it is 555.5499999999....

    // The good news is that we know any string of 33 or fewer decimal digits
    // can be converted to and from IEEE 754 binary128 without being changes

    // We make use of that here

    char ach[128];
    real_to_decimal (ach, TREE_REAL_CST_PTR (refer.field->data.value_of()),
                     sizeof(ach), 33, 0);
    char *p = strchr(ach, 'e');
    if( !p )
      {
      // Probably INF -INF NAN or -NAN, so ach has our result
      // Except that real_to_decimal prints -0.0 and 0.0 like that with
      // no e.
      if( ach[0] == '0' || ( ach[0] == '-' && ach[1] == '0' ))
        __gg__remove_trailing_zeroes(ach);
      }
    else
      {
      int exp = atoi(p+1);
      if( exp >= 6 || exp <= -5 )
        {
        // We are going to stick with the E notation, so ach has our result
        // Except that real_to_decimal prints with e notation rather than E
        // and doesn't guarantee at least two exponent digits.
        *p = 'E';
        if( exp < 0 && exp >= -9 )
          {
            p[1] = '-';
            p[2] = '0';
            p[3] = '0' - exp;
            p[4] = '\0';
          }
        else if( exp >= 0 && exp <= 9 )
          {
            p[1] = '+';
            p[2] = '0';
            p[3] = '0' + exp;
            p[4] = '\0';
          }
        }
      else if (exp == 0)
        {
          p[-1] = '\0';
        }
      else if (exp < 0)
        {
          p[-1] = '\0';
          char *q = strchr (ach, '.');
          char dig = q[-1];
          q[-1] = '\0';
          char tem[132];
          snprintf (tem, 132, "%s0.%0*u%c%s", ach, -exp - 1, 0, dig, q + 1);
          strcpy (ach, tem);
        }
      else if (exp > 0)
        {
          p[-1] = '\0';
          char *q = strchr (ach, '.');
          for (int i = 0; i != exp; ++i)
            q[i] = q[i + 1];
          q[exp] = '.';
        }
      __gg__remove_trailing_zeroes(ach);
      }

    if( symbol_decimal_point() == ',' )
      {
      char *p = strchr(ach, '.' );
      if( p )
        {
        *p = symbol_decimal_point();
        }
      }

    gg_write(  file_descriptor,
               gg_string_literal(ach),
               build_int_cst_type(SIZE_T, strlen(ach)));
    if( advance )
      {
      gg_write(  file_descriptor,
                 gg_string_literal("\n"),
                 integer_one_node);
      }
    }
  else
    {
    if( refer_is_clean(refer) )
      {
      gg_call(VOID,
              "__gg__display_clean",
              gg_get_address_of(refer.field->var_decl_node),
              file_descriptor,
              advance ? integer_one_node : integer_zero_node,
              NULL_TREE );
      }
    else
      {
      // We might be dealing with a refmod:
      if( refer.refmod.from || refer.refmod.len )
        {
        gg_attribute_bit_set(refer.field, refmod_e);
        }
      gg_call(VOID,
              "__gg__display",
              gg_get_address_of(refer.field->var_decl_node),
              refer_offset_source(refer),
              refer_size_source(  refer),
              file_descriptor,
              advance ? integer_one_node : integer_zero_node,
              NULL_TREE );
      if( refer.refmod.from || refer.refmod.len )
        {
        gg_attribute_bit_clear(refer.field, refmod_e);
        }
      }
    }
  cursor_at_sol = advance;
  }

void
parser_display_field(cbl_field_t *field)
  {
  parser_display_internal_field(integer_one_node,
                                field,
                                DISPLAY_NO_ADVANCE);
  }

void
parser_display( const struct cbl_special_name_t *upon,
                struct cbl_refer_t refs[],
                size_t n,
                bool advance )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" parser_display of multiple variables:")
    for(size_t i=0; i<n; i++)
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_REF("", refs[i]);
      }
    if( advance )
      {
      SHOW_PARSE_TEXT(" (advance)")
      }
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    for(size_t ii=0; ii<n; ii++)
      {
      if( ii != 0 )
        {
        TRACE1_INDENT
        }
      if(n > 1)
        {
        gg_fprintf(trace_handle, 1, "%ld: ", build_int_cst_type(INT, ii));
        }
      TRACE1_REFER("", refs[ii], "")
      }
    TRACE1_END
    }
  tree file_descriptor = gg_define_int();
  bool needs_closing = false;
  if( upon )
    {
    switch(upon->id)
      {
      case STDOUT_e:
      case SYSOUT_e:
      case SYSLIST_e:
      case SYSLST_e:
      case CONSOLE_e:
        gg_assign(file_descriptor, integer_one_node);
        break;

      case STDERR_e:
      case SYSPUNCH_e:
      case SYSPCH_e:
        gg_assign(file_descriptor, integer_two_node);
        break;

      default:
        if( upon->os_filename[0] )
          {
          tree topen = gg_open( gg_string_literal(upon->os_filename),
                                build_int_cst_type(INT, O_APPEND|O_WRONLY));
          gg_assign(file_descriptor, topen);
          needs_closing = true;
          }
        else
          {
          fprintf(stderr, "We don't know what to do in parser_display\n");
          gcc_unreachable();
          }
      }
    }
  else
    {
    gg_assign(file_descriptor,integer_one_node);   // stdout is file descriptor 1.
    }

  for(size_t i=0; i<n-1; i++)
    {
    CHECK_FIELD(refs[i].field);
    parser_display_internal(file_descriptor, refs[i], DISPLAY_NO_ADVANCE);
    }
  CHECK_FIELD(refs[n-1].field);
  parser_display_internal(file_descriptor, refs[n-1], advance ? DISPLAY_ADVANCE : DISPLAY_NO_ADVANCE);

  if( needs_closing )
    {
    tree tclose = gg_close(file_descriptor);
    // We are ignoring the close() return value
    gg_append_statement(tclose);
    }

  cursor_at_sol = advance;
  }

static tree
get_literalN_value(cbl_field_t *var)
  {
  // Get the literal N value from the integer var_decl
  tree retval = NULL_TREE;
  tree var_type = tree_type_from_size(var->data.capacity,
                                      var->attr & signable_e);
  retval = gg_cast(var_type, var->data_decl_node);
  return retval;
  }

void
parser_assign( size_t nC, cbl_num_result_t *C,
               struct cbl_refer_t sourceref,
               cbl_label_t *on_error,
               cbl_label_t *not_error,
               cbl_label_t *compute_error)
  {
  Analyze();
  RETURN_IF_PARSE_ONLY;
  // There might, or might not, already be error and/or not_error labels:
  set_up_on_exception_label(on_error);
  set_up_on_exception_label(not_error);
  set_up_compute_error_label(compute_error);

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    }

  TRACE1
    {
    TRACE1_HEADER
    char ach[32];
    sprintf(ach, "%ld target%s", nC, nC==1 ? "" : "s");
    TRACE1_TEXT(ach);
    if( on_error )
      {
      TRACE1_TEXT("; with on_error");
      }
    if( not_error )
      {
      TRACE1_TEXT("; with not_error");
      }
    }

  tree error_flag = gg_define_int(0);

  for(size_t i=0; i<nC; i++ )
    {
    TRACE1
      {
      char ach[48];
      sprintf(ach, "Processing target number %ld", i);
      TRACE1_INDENT
      TRACE1_TEXT(ach);
      }
    cbl_refer_t& destref( C[i].refer );
    cbl_round_t rounded = C[i].rounded;
    SHOW_PARSE
      {
      if(i)
        {
        SHOW_PARSE_INDENT
        }
      if( sourceref.field && is_figconst_low(sourceref.field) )
        {
        SHOW_PARSE_TEXT(" LOW-VALUE")
        }
      else if( sourceref.field && is_figconst_zero(sourceref.field) )
        {
        SHOW_PARSE_TEXT(" ZERO-VALUE")
        }
      else if( sourceref.field && is_figconst_space(sourceref.field) )
        {
        SHOW_PARSE_TEXT(" SPACE-VALUE")
        }
      else if( sourceref.field && is_figconst_quote(sourceref.field) )
        {
        SHOW_PARSE_TEXT(" QUOTE-VALUE")
        }
      else if( sourceref.field && is_figconst_high(sourceref.field) )
        {
        SHOW_PARSE_TEXT(" HIGH-VALUE")
        }
      else
        {
        SHOW_PARSE_REF(" ", sourceref)
        }
      SHOW_PARSE_REF(" TO ", destref)
      switch(rounded)
        {
        case away_from_zero_e:
          SHOW_PARSE_TEXT(" AWAY_FROM_ZERO")
          break;
        case nearest_toward_zero_e:
          SHOW_PARSE_TEXT(" NEAREST_TOWARD_ZERO")
          break;
        case toward_greater_e:
          SHOW_PARSE_TEXT(" TOWARD_GREATER")
          break;
        case toward_lesser_e:
          SHOW_PARSE_TEXT(" TOWARD_LESSER")
          break;
        case nearest_away_from_zero_e:
          SHOW_PARSE_TEXT(" NEAREST_AWAY_FROM_ZERO")
          break;
        case nearest_even_e:
          SHOW_PARSE_TEXT(" NEAREST_EVEN")
          break;
        case prohibited_e:
          SHOW_PARSE_TEXT(" PROHIBITED")
          break;
        case truncation_e:
          SHOW_PARSE_TEXT(" TRUNCATED")
          break;
        default:
          gcc_unreachable();
          break;
        }
      }

    CHECK_FIELD(destref.field);
    CHECK_FIELD(sourceref.field);

    // gg_printf("parser_assign: The compute_error_code is %d\n",
    //            gg_cast(INT, compute_error->structs.compute_error->compute_error_code), NULL_TREE);

    static tree erf = gg_define_variable(INT, "..pa_erf", vs_file_static);
    if( on_error )
      {
      // There is an ON ERROR clause.  When there is an ON ERROR clause, and
      // there is an error, the TARGET values are to be left unchanged.
      IF(compute_error->structs.compute_error->compute_error_code,
         ne_op,
         integer_zero_node )
        {
        // There was an error, so we do NOT replace the destref with the
        // sourceref value
        TRACE1
          {
          TRACE1_INDENT
          TRACE1_TEXT("on_error clause; computional error occurred")
          }
        }
      ELSE
        {
        TRACE1
          {
          TRACE1_INDENT
          TRACE1_TEXT("on_error clause; no computational error")
          }
        // There was no computational error.  Call the move routine that does
        // not replace the target when there is a size error:
        TREEPLET tsource;
        treeplet_fill_source(tsource, sourceref);
        static bool check_for_error = true;
        move_helper(erf,
                    destref,
                    sourceref,
                    tsource,
                    rounded,
                    check_for_error,
                    true);

        gg_assign(error_flag, gg_bitwise_or(error_flag, erf));
        IF(error_flag, ne_op, integer_zero_node)
          {
          TRACE1
            {
            TRACE1_INDENT
            TRACE1_TEXT("on_error clause; a move error occurred")
            }
          // There was an error during the move.  Set the exception status
          // information:
          gg_call(  VOID,
                    "__gg__process_compute_error",
                    build_int_cst_type(INT, compute_error_truncate),
                    NULL_TREE);
          // But because there is an ON ERROR clause, suppress DECLARATIVE
          // processing
          gg_assign(var_decl_exception_code, integer_zero_node);
          }
        ELSE
          {
          TRACE1
            {
            TRACE1_INDENT
            TRACE1_TEXT("on_error clause; no move")
            }
          }
        ENDIF
        }
      ENDIF
      }
    else
      {
      // There is no ON_ERROR clause, so we do the truncation type move, but
      // with one exception.  If the error was an exponentiation error that
      // resulted in a NaN, we *don't* do the move:

      IF( gg_bitwise_and( compute_error->structs.compute_error->compute_error_code,
                          build_int_cst_type(INT,
                                             compute_error_exp_minus_by_frac
                                             | compute_error_divide_by_zero)),
                          ne_op,
                          integer_zero_node )
        {
        // It was a NaN, so don't do the move
        TRACE1
          {
          TRACE1_INDENT
          TRACE1_TEXT("Not moving the NaN")
          }
        }
      ELSE
        {
        TRACE1
          {
          TRACE1_INDENT
          TRACE1_TEXT("Doing the move")
          }
        TREEPLET tsource;
        treeplet_fill_source(tsource, sourceref);
        static bool check_for_error = true;
        move_helper(erf,
                    destref,
                    sourceref,
                    tsource,
                    rounded,
                    check_for_error,
                    false);
        gg_assign(error_flag, gg_bitwise_or(error_flag, erf));
        IF(error_flag, ne_op, integer_zero_node)
          {
          // There was an error during the move.  Set the exception status
          // information:
          TRACE1
            {
            TRACE1_INDENT
            TRACE1_TEXT("Error during the move; calling __gg__process_compute_error")
            }
          gg_call(  VOID,
                    "__gg__process_compute_error",
                    build_int_cst_type(INT, compute_error_truncate),
                    NULL_TREE);
          }
        ELSE
          {
          }
        ENDIF
        }
      ENDIF
      }
    TRACE1
      {
      TRACE1_INDENT
      TRACE1_FIELD("source ", sourceref.field, "")
      TRACE1_INDENT
      TRACE1_FIELD("dest ", destref.field, "")
      TRACE1_END
      }
    }

  if( on_error )
    {
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT(" Laying down on_error GOTO into")
      SHOW_PARSE_LABEL(" ", on_error)
      }
    IF( gg_bitwise_or(error_flag,
                      compute_error->structs.compute_error->compute_error_code),
        ne_op,
        integer_zero_node )
      {
      gg_append_statement( on_error->structs.arith_error->into.go_to );
      }
    ELSE
      ENDIF
    }
  else
    {
    // We weren't given an explicit ON SIZE ERROR label, so we need to go
    // with the NO ERROR CLAUSE behavior
    if( compute_error )
      {
      gg_call(  VOID,
                "__gg__process_compute_error",
                compute_error->structs.compute_error->compute_error_code,
                NULL_TREE);
      }
    }

  if( not_error )
    {
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT(" Laying down not_error GOTO into")
      SHOW_PARSE_LABEL(" ", not_error)
      }
    IF( compute_error->structs.compute_error->compute_error_code, eq_op, integer_zero_node )
      {
      gg_append_statement( not_error->structs.arith_error->into.go_to );
      }
    ELSE
      ENDIF
    }

  if( on_error )
    {
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT(" Laying down on_error LABEL BOTTOM:")
      SHOW_PARSE_LABEL(" ", on_error)
      }
    gg_append_statement( on_error->structs.arith_error->bottom.label );
    }

  if( not_error )
    {
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT(" Laying down not_error LABEL BOTTOM:")
      SHOW_PARSE_LABEL(" ", not_error)
      }
    gg_append_statement( not_error->structs.arith_error->bottom.label );
    }

  SHOW_PARSE
    {
    SHOW_PARSE_END
    }
  }

static cbl_figconst_t
is_figconst(cbl_field_t *field)
  {
  cbl_figconst_t figconst = (cbl_figconst_t)(field->attr & FIGCONST_MASK);
  return figconst;
  }

static cbl_figconst_t
is_figconst(cbl_refer_t &sourceref)
  {
  return is_figconst(sourceref.field);
  }

void
parser_move(cbl_refer_t destref,
            cbl_refer_t sourceref,
            cbl_round_t rounded,
            bool skip_fill_from  // Defaults to false
            )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( sourceref.field && is_figconst_low(sourceref.field) )
      {
      SHOW_PARSE_TEXT(" LOW-VALUE")
      }
    else if( sourceref.field && is_figconst_zero(sourceref.field) )
      {
      SHOW_PARSE_TEXT(" ZERO-VALUE")
      }
    else if( sourceref.field && is_figconst_space(sourceref.field) )
      {
      SHOW_PARSE_TEXT(" SPACE-VALUE")
      }
    else if( sourceref.field && is_figconst_quote(sourceref.field) )
      {
      SHOW_PARSE_TEXT(" QUOTE-VALUE")
      }
    else if( sourceref.field && is_figconst_high(sourceref.field) )
      {
      SHOW_PARSE_TEXT(" HIGH-VALUE")
      }
    else
      {
      SHOW_PARSE_REF(" ", sourceref)
      }
    SHOW_PARSE_REF(" TO ", destref)
      switch(rounded)
        {
        case away_from_zero_e:
          SHOW_PARSE_TEXT(" AWAY_FROM_ZERO")
          break;
        case nearest_toward_zero_e:
          SHOW_PARSE_TEXT(" NEAREST_TOWARD_ZERO")
          break;
        case toward_greater_e:
          SHOW_PARSE_TEXT(" TOWARD_GREATER")
          break;
        case toward_lesser_e:
          SHOW_PARSE_TEXT(" TOWARD_LESSER")
          break;
        case nearest_away_from_zero_e:
          SHOW_PARSE_TEXT(" NEAREST_AWAY_FROM_ZERO")
          break;
        case nearest_even_e:
          SHOW_PARSE_TEXT(" NEAREST_EVEN")
          break;
        case prohibited_e:
          SHOW_PARSE_TEXT(" PROHIBITED")
          break;
        case truncation_e:
          SHOW_PARSE_TEXT(" TRUNCATED")
          break;
        default:
          gcc_unreachable();
          break;
        }
    SHOW_PARSE_END
    }

  if( !skip_fill_from )
    {
    cbl_figconst_t figconst = is_figconst(sourceref);
    if( figconst )
      {
      sourceref.all = true;
      }
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("About to call move_helper")
    }
  TREEPLET tsource;
  treeplet_fill_source(tsource, sourceref);
  static bool dont_check_for_error = false;
  move_helper(NULL, destref, sourceref, tsource, rounded, dont_check_for_error );

  TRACE1
    {
    TRACE1_INDENT
    TRACE1_REFER_INFO("source ", sourceref)
    TRACE1_INDENT
    TRACE1_REFER_INFO("dest   ", destref)
    TRACE1_END
    }
  }

static
void
parser_move_multi(cbl_refer_t destref,
                  cbl_refer_t sourceref,
                  TREEPLET    tsource,
                  cbl_round_t rounded,
                  bool skip_fill_from )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( sourceref.field && is_figconst_low(sourceref.field) )
      {
      SHOW_PARSE_TEXT(" LOW-VALUE")
      }
    else if( sourceref.field && is_figconst_zero(sourceref.field) )
      {
      SHOW_PARSE_TEXT(" ZERO-VALUE")
      }
    else if( sourceref.field && is_figconst_space(sourceref.field) )
      {
      SHOW_PARSE_TEXT(" SPACE-VALUE")
      }
    else if( sourceref.field && is_figconst_quote(sourceref.field) )
      {
      SHOW_PARSE_TEXT(" QUOTE-VALUE")
      }
    else if( sourceref.field && is_figconst_high(sourceref.field) )
      {
      SHOW_PARSE_TEXT(" HIGH-VALUE")
      }
    else
      {
      SHOW_PARSE_REF(" ", sourceref)
      }
    SHOW_PARSE_REF(" TO ", destref)
      switch(rounded)
        {
        case away_from_zero_e:
          SHOW_PARSE_TEXT(" AWAY_FROM_ZERO")
          break;
        case nearest_toward_zero_e:
          SHOW_PARSE_TEXT(" NEAREST_TOWARD_ZERO")
          break;
        case toward_greater_e:
          SHOW_PARSE_TEXT(" TOWARD_GREATER")
          break;
        case toward_lesser_e:
          SHOW_PARSE_TEXT(" TOWARD_LESSER")
          break;
        case nearest_away_from_zero_e:
          SHOW_PARSE_TEXT(" NEAREST_AWAY_FROM_ZERO")
          break;
        case nearest_even_e:
          SHOW_PARSE_TEXT(" NEAREST_EVEN")
          break;
        case prohibited_e:
          SHOW_PARSE_TEXT(" PROHIBITED")
          break;
        case truncation_e:
          SHOW_PARSE_TEXT(" TRUNCATED")
          break;
        default:
          gcc_unreachable();
          break;
        }
    SHOW_PARSE_END
    }

  if( !skip_fill_from )
    {
    cbl_figconst_t figconst = is_figconst(sourceref);
    if( figconst )
      {
      sourceref.all = true;
      }
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("About to call move_helper")
    }

  static bool dont_check_for_error = false;
  move_helper(NULL, destref, sourceref, tsource, rounded, dont_check_for_error );

  TRACE1
    {
    TRACE1_INDENT
    TRACE1_REFER_INFO("source ", sourceref)
    TRACE1_INDENT
    TRACE1_REFER_INFO("dest   ", destref)
    TRACE1_END
    }
  }

void
parser_move(size_t ntgt, cbl_refer_t *tgts, cbl_refer_t src, cbl_round_t rounded)
  {
  if( mode_syntax_only() ) return;

  cbl_figconst_t figconst = is_figconst(src);
  if( figconst )
    {
    src.all = true;
    }
  TREEPLET tsource;
  treeplet_fill_source(tsource, src);
  static const bool skip_fill_from = true;
  for( cbl_refer_t *p=tgts; p < tgts + ntgt; p++ )
    {
    parser_move_multi(*p, src, tsource, rounded, skip_fill_from);
    }
  }

/*
 * "nelem" represents the number of elements in the table.
 * "src" is the already-initialized first element of the table
 * to be initialized.  If nspan == 0, copy the whole record because
 * the record either has no filler, or WITH FILLER was specified.
 * Otherwise, the spans array comprises a set of {offset,end+1} pairs
 * representing sequences of consecutive non-FILLER fields.
 *
 * "table" is the symbol table index for the table being initialized.
 * It may appear in a subsequent call as part of the (sub)tbls array,
 * if it is nested in a higher-level table.
 */
void
parser_initialize_table(size_t nelem,
                        cbl_refer_t src,
                        size_t nspan,
                        const cbl_bytespan_t spans[],
                        size_t table, // symbol table index
                        size_t ntbl,
                        const cbl_subtable_t tbls[])
  {
  if( mode_syntax_only() ) return;

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_REFER("src:  ",  src, " ")
    TRACE1_END
    }
  typedef size_t span_t[2];
  static_assert(sizeof(spans[0]) == sizeof(span_t), "pair size wrong");
  static tree tspans = gg_define_variable(SIZE_T_P, "..pit_v1", vs_file_static);
  static tree ttbls  = gg_define_variable(SIZE_T_P, "..pit_v2", vs_file_static);
  gg_assign(tspans, build_array_of_size_t(2*nspan, (const size_t *)spans));
  gg_assign(ttbls,  build_array_of_size_t(2*ntbl,  (const size_t *)tbls));

  gg_call(VOID,
          "__gg__mirror_range",
          build_int_cst_type(SIZE_T, nelem),
          gg_get_address_of(src.field->var_decl_node),
          refer_offset_source(src),
          build_int_cst_type(SIZE_T, nspan),
          tspans,
          build_int_cst_type(SIZE_T, table),
          build_int_cst_type(SIZE_T, ntbl),
          ttbls,
          NULL_TREE);

  gg_free(tspans);
  gg_free(ttbls);
  }

static
tree
tree_type_from_field_type(cbl_field_t *field, size_t &nbytes)
  {
  /*  This routine is used to determine what action is taken with type of a
      CALL ... USING <var> and the matching PROCEDURE DIVISION USING <var> of
      a PROGRAM-ID or FUNCTION-ID
      */
  tree retval = COBOL_FUNCTION_RETURN_TYPE;
  nbytes = 8;
  if( field )
    {
    // This maps a Fldxxx to a C-style variable type:
    switch(field->type)
      {
      case FldGroup:
      case FldAlphanumeric:
      case FldAlphaEdited:
      case FldNumericEdited:
        retval = CHAR_P;
        nbytes = field->data.capacity;
        break;

      case FldNumericDisplay:
      case FldNumericBinary:
      case FldPacked:
        if( field->data.digits > 18 )
          {
          retval = UINT128;
          nbytes = 16;
          }
        else
          {
          retval = SIZE_T;
          nbytes = 8;
          }
        break;

      case FldNumericBin5:
      case FldIndex:
      case FldPointer:
        if( field->data.capacity > 8 )
          {
          retval = UINT128;
          nbytes = 16;
          }
        else
          {
          retval = SIZE_T;
          nbytes = 8;
          }
        break;

      case FldFloat:
        if( field->data.capacity == 8 )
          {
          retval = DOUBLE;
          nbytes = 8;
          }
        else if( field->data.capacity == 4 )
          {
          retval = FLOAT;
          nbytes = 4;
          }
        else
          {
          retval = FLOAT128;
          nbytes = 16;
          }
        break;

      case FldLiteralN:
        // Assume a 64-bit signed integer.  This happens for GOBACK STATUS 101,
        // the like
        retval = LONG;
        nbytes = 8;
        break;

      default:
        cbl_internal_error(  "%s(): Invalid field type %s:",
                __func__,
                cbl_field_type_str(field->type));
        break;
      }
    }
  if( retval == SIZE_T && field->attr & signable_e )
    {
    retval = SSIZE_T;
    }
  if( retval == UINT128 && field->attr & signable_e )
    {
    retval = INT128;
    }
  return retval;
  }

static void
restore_local_variables()
  {
  gg_call(VOID,
          "__gg__pop_local_variables",
          NULL_TREE);
  gg_decrement(var_decl_unique_prog_id);
  }

static inline bool
is_valuable( cbl_field_type_t type ) {
  switch ( type ) {
  case FldInvalid:
  case FldGroup:
  case FldAlphanumeric:
  case FldNumericEdited:
  case FldAlphaEdited:
  case FldLiteralA:
  case FldClass:
  case FldConditional:
  case FldForward:
  case FldSwitch:
  case FldDisplay:
  case FldBlob:
    return false;
  // These are variable types that have to be converted from their
  // COBOL form to a little-endian binary representation so that they
  // can be conveyed BY CONTENT/BY VALUE in a CALL or user-defined
  // function activation.
  case FldNumericDisplay:
  case FldNumericBinary:
  case FldFloat:
  case FldPacked:
  case FldNumericBin5:
  case FldLiteralN:
  case FldIndex:
  case FldPointer:
    return true;
  }
  cbl_internal_error( "%s:%d: invalid symbol_type_t %d", __func__, __LINE__, type );
  return false;
}

void parser_sleep(cbl_refer_t seconds)
  {
  if( seconds.field )
    {
    gg_get_address_of(seconds.field->var_decl_node);
    //refer_offset_source(seconds);
    //refer_size_source(seconds);

    gg_call(VOID,
            "__gg__sleep",
            gg_get_address_of(seconds.field->var_decl_node),
            refer_offset_source(seconds),
            refer_size_source(seconds),
            NULL_TREE);
    }
  else
    {
    // This is a naked place-holding CONTINUE.  Generate some do-nothing
    // code that will stick some .LOC information into the assembly language,
    // so that GDB-COBOL can display the CONTINUE statement.
    gg_assign(var_decl_nop, build_int_cst_type(INT, 103));
    }
  }

void
parser_exit_program(void)  // exits back to COBOL only, else continue
  {
  static cbl_label_t this_program = {};
  static cbl_refer_t magic_refer(&this_program, false);
  parser_exit( magic_refer );
  }

/*
 * If RETURNING was specified, the field is provided as an argument, no lookup
 * necessary.  refer.field == NULL means exit(0) unless ec != ec_none_e.
 * If ec == ec_all_e, that indicates RAISING LAST EXCEPTION was used.
 */

static
void
pe_stuff(cbl_refer_t refer, ec_type_t ec)
  {
  // This is the moral equivalent of a C "return xyz;".

  // There cannot be both a non-zero exit status and an exception condition.
  gcc_assert( !(ec != ec_none_e && refer.field != NULL) );

  gg_call(VOID,
          "__gg__pseudo_return_flush",
          NULL_TREE);

  cbl_field_t *returner = refer.field ? refer.field : current_function->returning;

  if( returner )
    {
    cbl_field_type_t field_type = returner->type;
    size_t nbytes = 0;
    tree return_type = tree_type_from_field_type(returner,
                                                 nbytes);
    tree retval   = gg_define_variable(return_type);

    gg_assign(retval, gg_cast(return_type, integer_zero_node));

    gg_modify_function_type(current_function->function_decl,
                            return_type);

    if( is_valuable( field_type ) )
      {
      // The field being returned is numeric.
      if(     field_type == FldNumericBin5
          ||  field_type == FldFloat
          ||  field_type == FldPointer
          ||  field_type == FldIndex )
        {
        // These are easily handled because they are all little-endian.
        gg_memcpy(gg_get_address_of(retval),
                  member(returner, "data"),
                  build_int_cst_type( SIZE_T,
                                      std::min(nbytes, (size_t)returner->data.capacity)));
        }
      else
        {
        // The field_type has a PICTURE string, so we need to convert from the
        // COBOL form to little-endian binary:
        tree value   = gg_define_int128();
        get_binary_value( value,
                          NULL,
                          returner,
                          size_t_zero_node);
        gg_memcpy(gg_get_address_of(retval),
                  gg_get_address_of(value),
                  build_int_cst_type(SIZE_T, nbytes));
        }
      restore_local_variables();
      gg_return(retval);
      }
    else
      {
      // The RETURNING type is a group or alphanumeric

      // The byte array to be returned is in returning, which is a local
      // variable on the stack.  We need to make a copy of it to avoid the
      // error of returning a pointer to data on the stack.

      tree array_type = build_array_type_nelts(UCHAR,
                                    returner->data.capacity);
      tree retval     =  gg_define_variable(array_type, vs_static);
      gg_memcpy(gg_get_address_of(retval),
                member(returner->var_decl_node, "data"),
                member(returner->var_decl_node, "capacity"));

      tree actual = gg_cast(COBOL_FUNCTION_RETURN_TYPE, gg_get_address_of(retval));

      restore_local_variables();
      gg_return(actual);
      }
    }
  else
    {
    // There is no explicit value.  This means, by default (according to)
    // IBM), we return the value found in RETURN-CODE:
    tree value = gg_define_variable(COBOL_FUNCTION_RETURN_TYPE);
    gg_assign(value,
              gg_cast(COBOL_FUNCTION_RETURN_TYPE,
                      var_decl_return_code));
    restore_local_variables();
    gg_return(gg_cast(COBOL_FUNCTION_RETURN_TYPE, value));
    }
  }

void
parser_exit( cbl_refer_t refer, ec_type_t ec )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if(    gg_trans_unit.function_stack.size()
        && current_function->returning
        && !refer.field)
      {
      // ->returning works only if there is no refer.field
      SHOW_PARSE_FIELD(" RETURNING ", current_function->returning);
      }
    if( gg_trans_unit.function_stack.size() && refer.field )
      {
      SHOW_PARSE_FIELD(" WITH STATUS ", refer.field);
      }
    if( gg_trans_unit.function_stack.size() && refer.prog_func )
      {
      SHOW_PARSE_TEXT(" refer.prog_func is non-zero")
      }

    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }

  if( refer.prog_func )
    {
    // We are processing EXIT PROGRAM.  If main() called us, we need to do
    // nothing.  Otherwise, this is a return
    IF( current_function->called_by_main_counter, eq_op, integer_zero_node )
      {
      // This function wasn't called by main, so we treat it like a GOBACK
      pe_stuff(refer, ec);
      }
    ELSE
      {
      // This function was called by main.  Is it the first call, or is it
      // recursive?
      IF( current_function->called_by_main_counter, gt_op, integer_one_node )
        {
        // This was a recursive call into the function originally called by
        // main.  Because we are under the control of a calling program, we
        // treat this like a GOBACK
        pe_stuff(refer, ec);
        }
      ELSE
        {
        // We are not under the control of a calling program, meaning that we
        // were called by main().  So, we do nothing, meaning we behave like
        // a CONTINUE.
        }
        ENDIF
      }
      ENDIF
    }
  else
    {
    IF( current_function->called_by_main_counter, gt_op, integer_zero_node )
      {
      // This wasn't an EXIT PROGRAM.  But in the case where we are the program
      // that was called by main(), we need to do some bookkeeping so that we
      // respond properly to an EXIT PROGRAM should one appear
      gg_decrement(current_function->called_by_main_counter);
      }
    ELSE
      {
      }
      ENDIF
    pe_stuff(refer, ec);
    }
  }

static void
walk_initialization(cbl_field_t *field, bool initialized, bool deallocate)
  {
  if( !(field->attr & based_e) )
    {
    // We are concerned only with BASED variables
    return;
    }
  symbol_elem_t *e = symbol_at(field_index(field));
  bool first_time = true;
  while( e < symbols_end() )
    {
    symbol_elem_t& element = *e++;
    if( element.type == SymField )
      {
      cbl_field_t *this_one = cbl_field_of(&element);
      if( !first_time )
        {
        if( this_one->level == LEVEL01 || this_one->level == LEVEL77 )
          {
          // Having encountered the next 01 or 77, we are done
          break;
          }
        }
      first_time = false;
      if( this_one->level == 00 )
        {
        // Ignore LEVEL00 "INDEXED BY" variables
        continue;
        }
      if(deallocate)
        {
        gg_assign(member(this_one->var_decl_node, "data"),
                  gg_cast(UCHAR_P, null_pointer_node));
        }
      else
        {
        gg_assign(member(this_one->var_decl_node, "data"),
                  gg_add(member(field->var_decl_node, "data"),
                          build_int_cst_type(SIZE_T, this_one->offset)));
        if(     this_one->level == 66
            ||  this_one->level == 88
            ||  symbol_redefines(this_one) )
          {
          continue;
          }
        if( !initialized )
          {
          // This is ALLOCATE Rule 9) in ISO 2023
          if( this_one->type == FldPointer )
            {
            gg_memset(member(this_one->var_decl_node, "data"),
                   integer_zero_node,
                   build_int_cst_type(SIZE_T, this_one->data.capacity));
            }
          }
        }
      }
    }
  }

void
parser_allocate(cbl_refer_t size_or_based,
                cbl_refer_t returning,
                bool initialized )
  {
  /*
   * If the 1st parameter has based_e attribute, the field it is based on defines
   * the number of bytes to allocate. In that case, "returning" is optional and
   * may have a NULL field.  Otherwise the 1st parameter is a numeric value and
   * allocated space is assigned to "returning", which is of type FldPointer.
   */

  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_REF(" size_or_based from:", size_or_based)
    SHOW_PARSE_INDENT
    SHOW_PARSE_REF("returning:         ", returning)
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_REFER("size_or_based: ", size_or_based, "");
    TRACE1_INDENT
    TRACE1_REFER("returning:     ", size_or_based, "");
    TRACE1_END
    }

  if( returning.field )
    {
    // If there is a returning, it has to be a pointer
    gcc_assert(returning.field->type == FldPointer);
    }

  if( !(size_or_based.field->attr & based_e) )
    {
    // If the first is not based, then there must be a returning
    gcc_assert(returning.field);
    }

  cbl_field_t *f_working = current_options().initial_working();
  cbl_field_t *f_local   = current_options().initial_local();

  int default_byte = wsclear() ? *wsclear() : -1;

  gg_call(VOID,
          "__gg__allocate",
          gg_get_address_of(size_or_based.field->var_decl_node),
          refer_offset_source(size_or_based) ,
          initialized ? integer_one_node : integer_zero_node,
          build_int_cst_type(INT, default_byte),
          f_working ? gg_get_address_of(f_working->var_decl_node) : null_pointer_node,
          f_local   ? gg_get_address_of(f_local->  var_decl_node) : null_pointer_node,
          returning.field ? gg_get_address_of(returning.field->var_decl_node)
                          : null_pointer_node,
          returning.field ? refer_offset_source(returning)
                          : size_t_zero_node,
          NULL_TREE);
  walk_initialization(size_or_based.field, initialized, false);
  }

void
parser_free( size_t n, cbl_refer_t refers[] )
  {
  if( mode_syntax_only() ) return; // Normally handled by SHOW_PARSE, if present

  Analyze();
  for( auto p = refers; p < refers + n; p++ )
    {
    gcc_assert( ! p->all );
    gcc_assert( ! p->is_refmod_reference() );
    if( !(p->field->type == FldPointer || p->addr_of || (p->field->attr & based_e)) )
      {
      dbgmsg("Deallocating %s means it has to be FldPointer or addr_of or based_e");
      }
    gcc_assert( p->field->type == FldPointer || p->addr_of || (p->field->attr & based_e) );

    gg_call(VOID,
            "__gg__deallocate",
            gg_get_address_of(p->field->var_decl_node),
            refer_offset_source(*p),
            p->addr_of ? integer_one_node : integer_zero_node,
            NULL_TREE);
    walk_initialization(p->field, false, true);
    }
  }

void
parser_arith_error(cbl_label_t *arithmetic_label)
  {
  // We can't use Analyze() on this one, because the exit ends up being laid
  // down before the enter when the goto logic gets untangled by the compiler.

  // We are entering either SIZE ERROR or NOT SIZE ERROR code
  RETURN_IF_PARSE_ONLY;
  set_up_on_exception_label(arithmetic_label);

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" Laying down GOTO OVER")
    SHOW_PARSE_LABEL(" ", arithmetic_label)
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT(" Laying down LABEL INTO:")
    SHOW_PARSE_LABEL(" ", arithmetic_label)
    SHOW_PARSE_END
    }

  // Jump over the [NOT] ON EXCEPTION code that is about to be laid down
  gg_append_statement( arithmetic_label->structs.arith_error->over.go_to );
  // Create the label that allows the following code to be executed at
  // when an ERROR, or NOT ERROR, has been determined to have taken place:
  gg_append_statement( arithmetic_label->structs.arith_error->into.label );
  }

void
parser_arith_error_end(cbl_label_t *arithmetic_label)
  {
  // We can't use Analyze() on this one, because the exit ends up being laid
  // down before the enter when the goto logic gets untangled by the compiler.

  // We have reached the end of the ERROR, or NOT ERROR, code.

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" Laying down GOTO BOTTOM")
    SHOW_PARSE_LABEL(" ", arithmetic_label)
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT(" Laying down LABEL OVER:")
    SHOW_PARSE_LABEL(" ", arithmetic_label)
    SHOW_PARSE_END
    }

  // Jump to the end of the arithmetic code:
  gg_append_statement( arithmetic_label->structs.arith_error->bottom.go_to );
  // Lay down the label that allows the ERROR/NOT ERROR instructions
  // to exist in a lacuna that doesn't get executed unless somebody jumps
  // to it:
  gg_append_statement( arithmetic_label->structs.arith_error->over.label );
  }

static void
propogate_linkage_offsets(cbl_field_t *field, tree base)
  {
  if( field->level == LEVEL01 || field->level == LEVEL77 )
    {
    field->data_decl_node = base;
    symbol_elem_t *e = symbol_at(field_index(field));
    // We already updated the data pointer of the first element:
    e += 1;
    while( e < symbols_end() )
      {
      symbol_elem_t& element = *e++;
      if( element.type == SymField )
        {
        cbl_field_t *this_one = cbl_field_of(&element);
        if( this_one->level == LEVEL01 || this_one->level == LEVEL77 )
          {
          // We have encountered another level 01/77.  If this LEVEL 01 had a
          // parent, then we have to assume that this is a redefines of another
          // level 01/77.
          if( this_one->parent )
            {
            // And, gloriously and frighteningly, it can be handled by
            // recursion:
            propogate_linkage_offsets(this_one, base);
            }
          else
            {
            // Having encountered the next 01 or 77, we are done
            break;
            }
          }
        if( this_one->level == 00 )
          {
          // Ignore LEVEL00 "INDEXED BY" variables
          continue;
          }
        tree offset = gg_define_variable(SIZE_T);
        IF( base, eq_op, gg_cast(UCHAR_P, null_pointer_node) )
          {
          gg_assign(offset, size_t_zero_node);
          }
        ELSE
          {
          gg_assign(offset, member(this_one, "offset"));
          }
        ENDIF
        this_one->data_decl_node = base;
        member( this_one,
                "data",
                gg_add(base, offset));
        }
      }
    }
  }

static bool initialized_data = false;
static void
initialize_the_data()
  {
  if( initialized_data )
    {
    return;
    }
  initialized_data = true;
  // Here is where we initialize the run-time list of currency symbols:
  const char *default_currency = "$";

  // This is one-time initialization of the libgcobol program state stack
  gg_call(VOID,
          "__gg__init_program_state",
          NULL_TREE);

  __gg__currency_signs = __gg__ct_currency_signs;
  // We initialize currency both at compile time and run time
  __gg__currency_sign_init();
  gg_call(VOID,
          "__gg__currency_sign_init",
          NULL_TREE);

  gg_call(VOID,
          "__gg__set_program_name",
          gg_string_literal( current_filename.back().c_str() ),
          NULL_TREE);

  for(int symbol=0; symbol<256; symbol++)
    {
    const char *sign = symbol_currency(symbol);
    if( sign )
      {
      default_currency = NULL;

      // Both compile-time and run-time
      __gg__currency_sign(symbol, sign);
      gg_call(VOID,
              "__gg__currency_sign",
              build_int_cst_type(INT, symbol),
              build_string_literal(strlen(sign)+1, sign),
              NULL_TREE);
      }
    }
  if( default_currency )
    {
    __gg__currency_sign(default_currency[0], default_currency);
    gg_call(VOID,
            "__gg__currency_sign",
            char_nodes[(int)default_currency[0]],
            gg_string_literal(default_currency),
            NULL_TREE);
    }

  // It's time to tell the library about DECIMAL-POINT IS COMMA:
  if( symbol_decimal_point() == ',' )
    {
    __gg__decimal_point     = ascii_comma ;
    __gg__decimal_separator = ascii_period ;
    gg_call(VOID,
            "__gg__decimal_point_is_comma",
            NULL_TREE);
    }
  }

void
parser_division(cbl_division_t division,
                cbl_field_t *returning,
                size_t nusing,
                cbl_ffi_arg_t args[] )
  {
  // This is called when the parser enters a COBOL program DIVISION.  See
  // parser_divide for the arithmetic operation.

  if( mode_syntax_only() ) return;

  // Do this before the SHOW_PARSE; it makes a little more sense when reviewing
  // the SHOW_PARSE output.
  if( division == identification_div_e )
    {
    initialized_data = false;
    if( gg_trans_unit.function_stack.size() >= 1 )
      {
      // This is a nested program.  So, we need to tie off the current
      // section:
      leave_paragraph_internal();
      leave_section_internal();
      }
    }

  if( division == environment_div_e )
    {
    initialized_data = false;
    }

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" ")
    switch(division)
      {
      case identification_div_e:
        SHOW_PARSE_TEXT("IDENTIFICATION")
        break;
      case environment_div_e:
        SHOW_PARSE_TEXT("ENVIRONMENT")
        break;
      case data_div_e:
        SHOW_PARSE_TEXT("DATA")
        break;
      case procedure_div_e:
        SHOW_PARSE_TEXT("PROCEDURE")
        break;
      }

    SHOW_PARSE_END
    }

  gg_set_current_line_number(CURRENT_LINE_NUMBER);

  if( division == data_div_e )
    {
    Analyze();
    initialize_the_data();
    }
  if( division == environment_div_e )
    {
    Analyze();
    initialize_the_data();
    }
  else if( division == procedure_div_e )
    {
    Analyze();
    initialize_the_data();

    // Do some symbol table index bookkeeping.  current_program_index() is valid
    // at this point in time:
    current_function->our_symbol_table_index = current_program_index();

    // We have some housekeeping to do to keep track of the list of functions
    // accessible by us:

    // For every procedure, we need a variable that points to the list of
    // available program names.

    // We need a pointer to the array of program names
    char ach[2*sizeof(cbl_name_t)];
    sprintf(ach,
            "..accessible_program_list_%ld",
            current_function->our_symbol_table_index);
    tree prog_list = gg_define_variable(build_pointer_type(CHAR_P),
                                        ach, vs_file_static);

    // Likewise, we need a pointer to the array of pointers to functions:
    tree function_type =
      build_varargs_function_type_array( SIZE_T,
                                         0,     // No parameters yet
                                         NULL); // And, hence, no types
    tree pointer_type = build_pointer_type(function_type);
    tree constructed_array_type = build_array_type_nelts(pointer_type, 1);
    sprintf(ach,
            "..accessible_program_pointers_%ld",
            current_function->our_symbol_table_index);
    tree prog_pointers = gg_define_variable(
                                    build_pointer_type(constructed_array_type),
                                    ach,
                                    vs_file_static);
    gg_call(VOID,
            "__gg__set_program_list",
            build_int_cst_type(INT, current_function->our_symbol_table_index),
            gg_get_address_of(prog_list),
            gg_get_address_of(prog_pointers),
            NULL_TREE);

    if( gg_trans_unit.function_stack.size() == 1 )
      {
      gg_create_goto_pair(&label_list_out_goto,
                          &label_list_out_label);
      gg_create_goto_pair(&label_list_back_goto,
                          &label_list_back_label);
      gg_append_statement(label_list_out_goto);
      gg_append_statement(label_list_back_label);
      }

    tree globals_are_initialized = gg_declare_variable( INT,
                                                        "__gg__globals_are_initialized",
                                                        NULL,
                                                        vs_external_reference);
    IF( globals_are_initialized, eq_op, integer_zero_node )
      {
      // one-time initialization happens here

      // We need to establish the initial value of the UPSI-1 switch register
      // We are using IBM's conventions:
      // https://www.ibm.com/docs/en/zvse/6.2?topic=SSB27H_6.2.0/fa2sf_communicate_appl_progs_via_job_control.html
      // UPSI 10000110 means that bits 0, 5, and 6 are on, which means that
      // SW-0, SW-5, and SW-6 are on.
      gg_call(VOID,
              "__gg__set_initial_switch_value",
              NULL_TREE);

      // And then flag one-time initialization as having been done.
      gg_assign(globals_are_initialized, integer_one_node);
      }
    ELSE
      ENDIF

    gg_append_statement(current_function->skip_init_label);
    // This is where we check to see if somebody tried to cancel us
    tree cancelled = gg_define_int();
    gg_assign(cancelled,
              gg_call_expr( INT,
                            "__gg__is_canceled",
                            gg_cast(SIZE_T,
                                    current_function->function_address),
                                    NULL_TREE));
    IF( cancelled, ne_op, integer_zero_node )
      {
      // Somebody flagged us for CANCEL, which means reinitialization, so we
      // need to find the _INITIALIZE_PROGRAM section label.

      // gg_printf("Somebody wants to cancel %s\n",
                // gg_string_literal(current_function->our_unmangled_name),
                // NULL_TREE);
      cbl_label_t *prog = cbl_label_of(symbol_at(current_program_index()));
      size_t initializer_index = prog->initial_section;
      cbl_label_t *initializer = cbl_label_of(symbol_at(initializer_index));
      parser_perform(initializer, true);  // true means suppress nexting
      }
    ELSE
      ENDIF

    // RETURNING variables are supposed to be in the linkage section, which
    // means that we didn't assign any storage to them during
    // parser_symbol_add().  We do that here.

    // returning also needs to behave like local storage, even though it is
    // in linkage.

    // This counter is used to help keep track of local variables
    gg_increment(var_decl_unique_prog_id);
    if( returning )
      {
      parser_local_add(returning);
      current_function->returning = returning;
      }

    // Stash the returning variables for use during parser_return()
    current_function->returning = returning;

    if( gg_trans_unit.function_stack.size() == 1 )
      {
      // We are entering a new top-level program, so we need to set
      // RETURN-CODE to zero
      gg_assign(var_decl_return_code, build_int_cst_type(SHORT, 0));
      }

    // The parameters passed to this program might be 64 bits or 128 bits in
    // length.  We establish those lengths based on the types of the target
    // for each USING.

    for(size_t i=0; i<nusing; i++)
      {
      // This code is relevant at compile time.  It takes each
      // expected formal parameter and tacks it onto the end of the
      // function's arguments chain.

      char ach[2*sizeof(cbl_name_t)];
      sprintf(ach, "_p_%s", args[i].refer.field->name);

      size_t nbytes = 0;
      tree par_type = tree_type_from_field_type(args[i].refer.field, nbytes);
      if( par_type == FLOAT )
        {
        par_type = SSIZE_T;
        }
      if( par_type == DOUBLE )
        {
        par_type = SSIZE_T;
        }
      if( par_type == FLOAT128 )
        {
        par_type = INT128;
        }
      chain_parameter_to_function(current_function->function_decl, par_type, ach);
      }

    bool check_for_parameter_count = false;

    if( nusing )
      {
      // During the call, we saved the parameter_count and an array of variable
      // lengths.  We need to look at those values if, and only if, one or more
      // of our USING arguments has an OPTIONAL flag or if one of our targets is
      // marked as VARYING.
      for(size_t i=0; i<nusing; i++)
        {
        if( args[i].optional )
          {
          check_for_parameter_count = true;
          break;
          }
        if( args[i].refer.field->attr & any_length_e )
          {
          check_for_parameter_count = true;
          break;
          }
        }

      if( check_for_parameter_count )
        {
        IF( var_decl_call_parameter_signature,
            eq_op,
            gg_cast(CHAR_P, current_function->function_address) )
          {
          // We know to use var_decl_call_parameter_count, so unflag this
          // pointer to avoid problems in the ridiculous possibility of
          // COBOL-A calls C_B calls COBOL_A
          gg_assign(var_decl_call_parameter_signature,
                    gg_cast(CHAR_P, null_pointer_node));
          }
        ELSE
          {
          // We were apparently called by a C routine, not a COBOL routine, so
          // make sure we don't get shortchanged by a count left behind from an
          // earlier COBOL call.
          gg_assign(var_decl_call_parameter_count,
                    build_int_cst_type(INT, A_ZILLION));
          }
        ENDIF
        }
      else
        {
        // None of our parameters require a count, so make sure we don't get
        // bamboozled by a count left behind from an earlier COBOL call.
        gg_assign(var_decl_call_parameter_count,
                  build_int_cst_type(INT, A_ZILLION));
        }

      // There are 'nusing' elements in the PROCEDURE DIVISION USING list.

      tree parameter;
      tree rt_i = gg_define_int();
      for(size_t i=0; i<nusing; i++)
        {
        // And this compiler code generates run-time execution code. The
        // generated code picks up, at run time, the variable we just
        // established in the chain at compile time.

        // It makes more sense if you don't think about it too hard.

        // We need to be able to restore prior arguments when doing recursive
        // calls:
        IF( member(args[i].refer.field->var_decl_node, "data"),
            ne_op,
            gg_cast(UCHAR_P, null_pointer_node) )
          {
          gg_call(VOID,
                  "__gg__push_local_variable",
                  gg_get_address_of(args[i].refer.field->var_decl_node),
                  NULL_TREE);
          }
        ELSE
          ENDIF

        tree base = gg_define_variable(UCHAR_P);
        gg_assign(rt_i, build_int_cst_type(INT, i));
        //gg_printf("The rt_i counter is %d\n", rt_i, NULL_TREE);
        IF( rt_i, lt_op , var_decl_call_parameter_count )
          {
          if( i == 0 )
            {
            // This is the first parameter.
            parameter = DECL_ARGUMENTS(current_function->function_decl);
            }
          else
            {
            // These are subsequent parameters
            parameter = TREE_CHAIN(parameter);
            }
          gg_assign(base, gg_cast(UCHAR_P, parameter));

          if( args[i].refer.field->attr & any_length_e )
            {
            // gg_printf("side channel: Length of \"%s\" is %ld\n", 
                      // member(args[i].refer.field->var_decl_node, "name"),
                      // gg_array_value(var_decl_call_parameter_lengths, rt_i), 
                      // NULL_TREE);

            // Get the length from the global lengths[] side channel.  Don't
            // forget to use the length mask on the table value.
            gg_assign(member(args[i].refer.field->var_decl_node, "capacity"),
                      gg_array_value(var_decl_call_parameter_lengths, rt_i));
            }
          }
        ELSE
          {
          gg_assign(base, gg_cast(UCHAR_P, null_pointer_node));
          }
          ENDIF

        // Arriving here means that we are processing an instruction like
        // this:
        // PROCEDURE DIVISION USING using[0] using[1] ... using using[nusing-1]

        // When __gg__call_parameter_count is equal to A_ZILLION, then this is
        // an OTHER-TO-COBOL call and the var_decl_call_parameter_lengths array
        // is not valid

        cbl_ffi_crv_t crv = args[i].crv;
        cbl_field_t *new_var = args[i].refer.field;

        if( crv == by_value_e )
          {
          switch(new_var->type)
            {
            case FldGroup:
            case FldAlphanumeric:
            case FldAlphaEdited:
            case FldNumericEdited:
              crv = by_reference_e;
              break;
            default:
              break;
            }
          }

        if( crv == by_value_e )
          {
          // 'parameter' is the 64-bit or 128-bit value that was placed on the stack

          size_t nbytes;
          tree_type_from_field_type(new_var, nbytes);
          tree parm = gg_define_variable(INT128);

          if( nbytes <= 8 )
            {
            // Our input is a 64-bit number
            if( new_var->attr & signable_e )
              {
              IF( gg_bitwise_and( gg_cast(SIZE_T, base),
                                  build_int_cst_type(SIZE_T, 0x8000000000000000ULL)),
                  ne_op,
                  gg_cast(SIZE_T, integer_zero_node) )
                {
                // Our input is a negative number
                gg_assign(parm, gg_cast(INT128, integer_minus_one_node));
                }
              ELSE
                {
                // Our input is a positive number
                gg_assign(parm, gg_cast(INT128, integer_zero_node));
                }
              ENDIF
              }
            else
              {
              // This is a 64-bit positive number:
              gg_assign(parm, gg_cast(INT128, integer_zero_node));
              }
            }
          // At this point, parm has been set to 0 or -1

          gg_memcpy(gg_get_address_of(parm),
                    gg_get_address_of(base),
                    build_int_cst_type(SIZE_T, nbytes));

          tree array_type = build_array_type_nelts(UCHAR, new_var->data.capacity);
          tree data_decl_node = gg_define_variable( array_type,
                                                    NULL,
                                                    vs_static);
          gg_assign( member(new_var->var_decl_node, "data"),
                            gg_get_address_of(data_decl_node) );

          // And then move it into place
          gg_call(VOID,
                  "__gg__assign_value_from_stack",
                  gg_get_address_of(new_var->var_decl_node),
                  parm,
                  NULL_TREE);
          // We now have to handle an oddball situation.  It's possible we are
          // dealing with
          //
          //    linkage section.
          //    01 var1
          //    01 var2 redefines var1
          //
          // If so, we have to give var2::data_pointer the same value as
          // var1::data_pointer
          //
          cbl_field_t *next_var;
          size_t our_index = symbol_index(symbol_elem_of(new_var));
          size_t next_index  = our_index + 1;
          // Look ahead in the symbol table for the next LEVEL01/77
          for(;;)
            {
            symbol_elem_t *e = symbol_at(next_index);
            if( e->type != SymField )
              {
              break;
              }
            next_var = cbl_field_of(e);
            if( !next_var )
              {
              break;
              }
            if( next_var->level == LEVEL01 || next_var->level == LEVEL77 )
              {
              if( next_var->parent == our_index )
                {
                gg_assign(member(next_var->var_decl_node, "data"),
                          member(new_var->var_decl_node, "data"));
                }
              break;
              }
            next_index += 1;
            }
          }
        else
          {
          // 'parameter' is a reference, so it it becomes the data member of
          // the cblc_field_t COBOL variable.
          gg_assign(member(args[i].field()->var_decl_node, "data"), base);

          // We need to apply base + offset to the LINKAGE variable
          // and all of its children
          propogate_linkage_offsets( args[i].field(), base );
          }
        }
      }

    gg_call(VOID,
            "__gg__pseudo_return_bookmark",
          NULL_TREE);

    // The MODULE-NAME function requires a stack of program names. We push the
    // name on here.  The first character is a 'T' or an 'N', where 'N' means
    // this is a nested program.

    if( gg_trans_unit.function_stack.size() > 1 )
      {
      // This is a nested program
      strcpy(ach, "N");
      }
    else
      {
      // This is a top-level program:
      strcpy(ach, "T");
      }
    strcat(ach, current_function->our_unmangled_name);
    gg_call(VOID,
            "__gg__module_name_push",
            gg_string_literal(ach),
            NULL_TREE);

    IF( var_decl_main_called, ne_op, integer_zero_node )
      {
      // We were just called by main:
      gg_assign(var_decl_main_called, integer_zero_node);
      gg_assign(current_function->called_by_main_counter, integer_one_node);
      }
    ELSE
      {
      // This isn't a call from main(), but it might be a recursive call to the
      // function that was called by main:
      IF(current_function->called_by_main_counter, ne_op, integer_zero_node)
        {
        // In that case, we bump the counter to keep track of things.
        gg_increment(current_function->called_by_main_counter);
        }
      ELSE
        {
        }
        ENDIF
      }
      ENDIF
    }
  }

void
parser_logop( struct cbl_field_t *tgt,
              struct cbl_field_t *a, // Is NULL for single-valued ops
              enum logop_t logop,
              struct cbl_field_t *b )
  {
  Analyze();
  SHOW_PARSE
    {
    if( logop == true_op)
      {
      SHOW_PARSE_HEADER
      SHOW_PARSE_FIELD(" ", tgt)
      SHOW_PARSE_TEXT(" will be set to TRUE ")
      }
    else if( logop == false_op)
      {
      SHOW_PARSE_HEADER
      SHOW_PARSE_FIELD(" ", tgt)
      SHOW_PARSE_TEXT(" will be set to FALSE ")
      }
    else
      {
      SHOW_PARSE_HEADER
      SHOW_PARSE_FIELD(" ", tgt)
      SHOW_PARSE_TEXT(" = ")
      if( a )
        {
        SHOW_PARSE_FIELD("", a)
        }
      SHOW_PARSE_TEXT(" ")
      SHOW_PARSE_TEXT( cbl_logop_str(logop) )
      if( b )
        {
        SHOW_PARSE_FIELD(" ", b)
        }
      }
    SHOW_PARSE_END
    }

  CHECK_FIELD(tgt);
  switch(logop)
    {
    case and_op:
    case or_op:
    case xor_op:
    case xnor_op:
    case not_op:
      CHECK_FIELD(b);
      break;
    default:
      break;
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT_ABC("operation: ", cbl_logop_str(logop), "")
    TRACE1_END
    if( logop != true_op )
      {
      if( a )
        {
        TRACE1_INDENT
        TRACE1_FIELD("operand A: ", a, "");
        }
      TRACE1_INDENT
      if( b )
        {
        TRACE1_FIELD("operand B: ", b, "");
        }
      TRACE1_END
      }
    }

  switch(logop)
    {
    case and_op:
    case or_op:
    case xor_op:
    case xnor_op:
      CHECK_FIELD(a);
      break;
    default:
      break;
    }

  // This routine takes two conditionals and a logical operator.  From those,
  // it creates and returns another conditional:

  if( tgt->type != FldConditional )
    {
    cbl_internal_error("parser_logop() was called with variable %s on line %d"
          ", which is not a FldConditional\n",
          tgt->name, cobol_location().first_line);
    }
  if( a && a->type != FldConditional )
    {
    cbl_internal_error("parser_logop() was called with variable %s on line %d"
          ", which is not a FldConditional\n",
          a->name, cobol_location().first_line);
    }
  if( b && b->type != FldConditional )
    {
    cbl_internal_error("parser_logop() was called with variable %s on line %d"
          ", which is not a FldConditional\n",
          b->name, cobol_location().first_line);
    }

  switch( logop )
    {
    case and_op:
      gg_assign(tgt->var_decl_node, gg_build_logical_expression(
                  a->var_decl_node,
                  and_op,
                  b->var_decl_node));
      break;

    case or_op:
      gg_assign(tgt->var_decl_node, gg_build_logical_expression(
                  a->var_decl_node,
                  or_op,
                  b->var_decl_node));
      break;

    case not_op:
      gg_assign(tgt->var_decl_node, gg_build_logical_expression(
                  NULL,
                  not_op,
                  b->var_decl_node));
      break;

    case xor_op:
      gg_assign(tgt->var_decl_node, gg_build_logical_expression(
                  a->var_decl_node,
                  xor_op,
                  b->var_decl_node));
      break;

    case xnor_op:
      {
      gg_assign(  tgt->var_decl_node,
                  gg_build_logical_expression(a->var_decl_node,
                                              xor_op,
                                              b->var_decl_node));

      // I need to negate the result.

      gg_assign(tgt->var_decl_node, gg_build_logical_expression(
                  NULL,
                  not_op,
                  tgt->var_decl_node));
      }
    break;

    case true_op:
      gg_assign(tgt->var_decl_node, boolean_true_node);
      break;

    case false_op:
      gg_assign(tgt->var_decl_node, boolean_false_node);
      break;
    }

  TRACE1
    {
    TRACE1_INDENT
    TRACE1_TEXT_ABC("result: ", tgt->name, "")
    TRACE1_FIELD_VALUE("", tgt, "")
    TRACE1_END
    }
  }

void
parser_relop(   cbl_field_t *tgt,
                cbl_refer_t aref,
                enum relop_t relop,
                cbl_refer_t bref )
  {
  Analyze();
  cbl_field_t *a = aref.field, *b = bref.field;
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD(" ", tgt)
    SHOW_PARSE_REF(" = ", aref)
    SHOW_PARSE_TEXT(" ")
    SHOW_PARSE_TEXT(relop_str(relop))
    SHOW_PARSE_REF(" ", bref)
    SHOW_PARSE_END
    }

  CHECK_FIELD(tgt);
  CHECK_FIELD(a);
  CHECK_FIELD(b);

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT_ABC("operation: ", relop_str(relop), "")
    TRACE1_INDENT
    TRACE1_REFER("operand A: ", aref, "");
    TRACE1_INDENT
    TRACE1_REFER("operand B: ", bref, "");
    }

  // This routine builds the relational expression and returns the TREE as
  // a conditional:

  if( tgt->type != FldConditional )
    {
    cbl_internal_error("parser_relop() was called with variable %s, "
          "which is not a FldConditional\n",
          tgt->name);
    }

  static tree comp_res = gg_define_variable(INT, "..pr_comp_res", vs_file_static);
  cobol_compare(comp_res, aref, bref);

  // comp_res is negative, zero, position for less-than, equal-to, greater-than

  // So, we simply compare the result of the comparison to zero using the relop
  // we were given to turn it into a TRUE/FALSE
  gg_assign(  tgt->var_decl_node,
              gg_build_relational_expression( comp_res,
                  relop,
                  integer_zero_node));
  TRACE1
    {
    TRACE1_INDENT
    TRACE1_FIELD("result: ", tgt, "")
    TRACE1_END
    }
  }

void
parser_relop_long(cbl_field_t *tgt,
                  long avalue,
                  enum relop_t relop,
                  cbl_refer_t bref )
  {
  Analyze();
  // We are comparing a long to a field, so the field had best be numerical

  cbl_field_t *b = bref.field;
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD(" ", tgt)
    SHOW_PARSE_TEXT(" = <long value> ")
    SHOW_PARSE_TEXT(relop_str(relop))
    SHOW_PARSE_REF(" ", bref)
    SHOW_PARSE_END
    }

  CHECK_FIELD(tgt);
  CHECK_FIELD(b);

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT_ABC("operation: ", relop_str(relop), "")
    TRACE1_INDENT
    char ach[32];
    sprintf(ach, "operand A: %ld (long value) ", avalue);
    TRACE1_TEXT(ach);
    TRACE1_INDENT
    TRACE1_REFER("operand B: ", bref, "");
    }

  // This routine builds the relational expression and returns the TREE as
  // a conditional:

  if( tgt->type != FldConditional )
    {
    cbl_internal_error("parser_relop() was called with variable %s, "
          "which is not a FldConditional\n",
          tgt->name);
    }

  tree tree_a  = build_int_cst_type(LONG, avalue);
  static tree tree_b  = gg_define_variable(LONG, "..prl_tree_b", vs_file_static);
  get_binary_value( tree_b,
                    NULL,
                    bref.field,
                    refer_offset_source(bref) );

  static tree comp_res = gg_define_variable(LONG, "..prl_comp_res", vs_file_static);
  gg_assign(comp_res, gg_subtract(tree_a, tree_b));

  // comp_res is negative, zero, position for less-than, equal-to, greater-than

  // So, we simply compare the result of the comparison to zero using the relop
  // we were given to turn it into a TRUE/FALSE
  gg_assign(  tgt->var_decl_node,
              gg_build_relational_expression( comp_res,
                                            relop,
                                            gg_cast(LONG, integer_zero_node)));
  TRACE1
    {
    TRACE1_INDENT
    TRACE1_FIELD("result: ", tgt, "")
    TRACE1_END
    }
  }

void
parser_if( struct cbl_field_t *conditional )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD(" ", conditional)
    SHOW_PARSE_END
    }

  CHECK_FIELD(conditional);

  if( conditional->type != FldConditional )
    {
    cbl_internal_error("parser_if() was called with variable %s, "
          "which is not a FldConditional\n",
          conditional->name);
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("testing: ")
    TRACE1_TEXT(conditional->name)
    TRACE1_FIELD_VALUE("", conditional, "")
    TRACE1_END
    }

  gg_create_true_false_statement_lists(conditional->var_decl_node);
  }

// The following routines border on abuse of the preprocessor, if not the
// programmer who is trying to understand this.  Look at the #defines in
// gengen.h, and check out the comments for gg_if in gengen.c

void
parser_else(void)
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  ELSE
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("taking FALSE branch")
    TRACE1_END
    }
  }

void
parser_fi(void)
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  ENDIF
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }
  }

void
parser_see_stop_run(struct cbl_refer_t exit_status,
                    const char *message)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( exit_status.field )
      {
      SHOW_PARSE_FIELD(" ERROR STATUS ", exit_status.field);
      }
    SHOW_PARSE_END
    }
  if( message )
    {
    parser_display_literal(message, DISPLAY_ADVANCE);
    }
  TRACE1
    {
    TRACE1_HEADER
    }

  // It's a stop run.  Return return-code to the operating system:
  static tree returned_value = gg_define_variable(INT, "..pssr_retval", vs_file_static);

  if( exit_status.field )
    {
    // There is an exit_status, so it wins:
    get_binary_value( returned_value,
                      NULL,
                      exit_status.field,
                      refer_offset_source(exit_status));
    TRACE1
      {
      TRACE1_REFER(" exit_status ", exit_status, "")
      }
    }
  else
    {
    gg_assign(returned_value, gg_cast(INT, var_decl_return_code));
    TRACE1
      {
      gg_fprintf( trace_handle,
                  2,
                  "RETURN-CODE %s [%d]",
                  gg_string_literal(cbl_field_of(
                                    symbol_at(return_code_register()))->name),
                  returned_value);
      }
    }
  TRACE1
    {
    gg_printf(" gg_exit(%d)\n", returned_value, NULL_TREE);
    TRACE1_END
    }
  gg_exit(returned_value);
  }

static
cbl_label_addresses_t *
label_fetch(struct cbl_label_t *label)
  {
  if( !label->structs.goto_trees )
    {
    label->structs.goto_trees
      = (cbl_label_addresses_t *)xmalloc(sizeof(struct cbl_label_addresses_t) );

    gg_create_goto_pair(&label->structs.goto_trees->go_to,
                        &label->structs.goto_trees->label);
    }
  return label->structs.goto_trees;
  }

void
parser_label_label(struct cbl_label_t *label)
  {
  label->lain = yylineno;
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_LABEL("", label)
    char ach[32];
    sprintf(ach, " label is at %p", (void*)label);
    SHOW_PARSE_TEXT(ach)
    sprintf(ach, " label->proc is %p", (void*)label->structs.proc);
    SHOW_PARSE_TEXT(ach)
    SHOW_PARSE_END
    }

  CHECK_LABEL(label);

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_LABEL("Establish label: ", label, "")
    TRACE1_END
    }

  if(strcmp(label->name, "_end_declaratives") == 0 )
    {
    suppress_cobol_entry_point = false;
    }
  gg_append_statement( label_fetch(label)->label );
  }

void
parser_label_goto(struct cbl_label_t *label)
  {
  label->used = yylineno;
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_LABEL(" ", label)
    char ach[32];
    sprintf(ach, " label is at %p", (void*)label);
    SHOW_PARSE_TEXT(ach)
    sprintf(ach, " label->proc is %p", (void*)label->structs.proc);
    SHOW_PARSE_TEXT(ach)
    SHOW_PARSE_END
    }

  CHECK_LABEL(label);

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_LABEL("GOTO label: ", label, "")
    TRACE1_END
    }

  if(strcmp(label->name, "_end_declaratives") == 0 )
    {
    suppress_cobol_entry_point = true;
    }

  gg_append_statement( label_fetch(label)->go_to );
  }

void
parser_setop( struct cbl_field_t *tgt,
              struct cbl_field_t *candidate,
              enum setop_t op,
              struct cbl_field_t *domain)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD(" ", tgt)
    SHOW_PARSE_FIELD(" = ", candidate)
    if( op == is_op )
      {
      SHOW_PARSE_TEXT(" is_op ")
      }
    SHOW_PARSE_FIELD(" = ", domain)
    SHOW_PARSE_END
    }

  CHECK_FIELD(tgt);
  CHECK_FIELD(candidate);
  CHECK_FIELD(domain);

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_FIELD("parser_setop: ", candidate, "")
    TRACE1_TEXT(" ")
    TRACE1_TEXT(setop_str(op))
    TRACE1_FIELD(" ", domain, "")
    TRACE1_END
    }

  gcc_assert(tgt->type == FldConditional);
  gcc_assert(domain->data.initial);
  gcc_assert(strlen(domain->data.initial));

  switch(op)
    {
    case is_op:
      switch(candidate->type)
        {
        case FldGroup:
        case FldAlphanumeric:
          gg_assign(tgt->var_decl_node, gg_build_relational_expression(
                      gg_call_expr(INT,
                                   "__gg__setop_compare",
                                   member(candidate, "data"),
                                   member(candidate, "capacity"),
                                   member(domain, "initial"),
                                   NULL_TREE),
                      ne_op,
                      integer_zero_node));
          break;
        default:
          dbgmsg("###### %10s in %s:%d\n", __func__, __FILE__, __LINE__ );
          cbl_internal_error(
                "###### candidate %s has unimplemented CVT_type %d(%s)\n",
                candidate->name,
                candidate->type,
                cbl_field_type_str(candidate->type));
          gcc_unreachable();
          break;
        }
      break;

    default:
      dbgmsg("###### %10s in %s:%d\n", __func__, __FILE__, __LINE__ );
      cbl_internal_error("###### unknown setop_t code %d\n", op);
      gcc_unreachable();
      break;
    }
  }

void
parser_classify(    cbl_field_t *tgt,
                    cbl_refer_t  candidate,
                    enum classify_t type )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD(" ", tgt)
    SHOW_PARSE_FIELD(" = ", candidate.field)
    SHOW_PARSE_TEXT(" IS ")
    SHOW_PARSE_TEXT(classify_str(type))
    SHOW_PARSE_END
    }

  gcc_assert(tgt->type == FldConditional);

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_REFER_VALUE("parser_classify: ", candidate, "")
    TRACE1_TEXT(" ")
    TRACE1_TEXT(classify_str(type))
    }

  gg_assign(tgt->var_decl_node, gg_build_relational_expression(
              gg_call_expr(INT,
                           "__gg__classify",
                           build_int_cst_type(INT, type),
                           gg_get_address_of(candidate.field->var_decl_node),
                           refer_offset_dest(candidate),
                           refer_size_dest(candidate),
                           NULL_TREE),
              ne_op,
              integer_zero_node));

  TRACE1
    {
    TRACE1_TEXT(" result is ")
    TRACE1_TEXT(tgt->name)
    TRACE1_FIELD_VALUE(" -> ", tgt, "")
    TRACE1_END
    }
  }

void
parser_perform(struct cbl_perform_tgt_t *tgt, struct cbl_refer_t how_many)
  {
  cbl_field_t *N = how_many.field;
  // No SHOW_PARSE here; we want to fall through:
  if( !tgt->to() )
    {
    // We only have tgt->from.
    if( !N )
      {
      // There is no N.  This is a simple PERFORM proc-1
      parser_perform(tgt->from());
      }
    else
      {
      // This is a PERFORM proc-1 N TIMES
      parser_perform_times(tgt->from(), how_many);
      }
    }
  else
    {
    // We have both from and to
    if( !N )
      {
      // There is no N.  This is PERFORM proc-1 THROUGH proc-2
      // false means nexting in GDB will work
      internal_perform_through(tgt->from(), tgt->to(), false);
      }
    else
      {
      // This is a PERFORM proc-1 THROUGH proc-2 N TIMES
      internal_perform_through_times(tgt->from(), tgt->to(), how_many);
      }
    }
  }

static void
create_iline_address_pairs(struct cbl_perform_tgt_t *tgt)
  {
  gg_create_goto_pair(&tgt->addresses.top.go_to,
                      &tgt->addresses.top.label);

  gg_create_goto_pair(&tgt->addresses.exit.go_to,
                      &tgt->addresses.exit.label);

  gg_create_goto_pair(&tgt->addresses.test.go_to,
                      &tgt->addresses.test.label);

  gg_create_goto_pair(&tgt->addresses.testA.go_to,
                      &tgt->addresses.testA.label);

  gg_create_goto_pair(&tgt->addresses.setup.go_to,
                      &tgt->addresses.setup.label);

  // Even in -O0 compilations, the compiler does some elementary optimizations
  // around JMP instructions.  We have the SETUP code for in-line performats
  // in an island at the end of the loop code.  With this intervention, NEXTing
  // through the code shows you the final statement of the loop before the
  // loop actually starts.

  tgt->addresses.line_number_of_setup_code = gg_get_current_line_number();
  }

void
parser_perform_start( struct cbl_perform_tgt_t *tgt )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( tgt )
      {
      SHOW_PARSE_TEXT(" cbl_perform_tgt_t is at")
      char ach[32];
      sprintf(ach, " %p", (void*)tgt);
      SHOW_PARSE_TEXT(ach);
      SHOW_PARSE_LABEL(" ", tgt->from())
      if( tgt->to() )
        {
        SHOW_PARSE_LABEL(" ", tgt->to())
        }
      }
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    if( tgt->from() )
      {
      TRACE1_LABEL(" from ", tgt->from(), "")
      }
    if( tgt->to() )
      {
      TRACE1_LABEL(" to ", tgt->to(), "")
      }
    TRACE1_END
    }

  // Create the goto/label pairs we are going to be needing:
  create_iline_address_pairs(tgt);

  // From here we have to jump to the loop setup code:
  SHOW_PARSE
    {
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT("GOTO SETUP")
    SHOW_PARSE_END
    }
  gg_append_statement(tgt->addresses.setup.go_to);

  // The next parser+_generated instructions will be the body of the loop, so we
  // need a TOP label here so we can get back to them:
  SHOW_PARSE
    {
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT("LABEL TOP:")
    SHOW_PARSE_END
    }

  // Give GDB-COBOL something to chew on when NEXTing.  This instruction will
  // get the line number of the PERFORM N TIMES code.
  gg_append_statement(tgt->addresses.top.label);
  gg_assign(var_decl_nop, build_int_cst_type(INT, 104));
  }

void
parser_perform_conditional( struct cbl_perform_tgt_t *tgt )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" cbl_perform_tgt_t is at")
    char ach[32];
    sprintf(ach, " %p", (void*)tgt);
    SHOW_PARSE_TEXT(ach);
    SHOW_PARSE_END
    }

  size_t i = tgt->addresses.number_of_conditionals;

  if( !(i < MAXIMUM_UNTILS) )
    {
    cbl_internal_error("%s:%d: %zu exceeds MAXIMUM_UNTILS of %d, line %d",
          __func__, __LINE__, i, MAXIMUM_UNTILS, CURRENT_LINE_NUMBER);
    }
  gcc_assert(i < MAXIMUM_UNTILS);

  // Create an unnamed goto/label pair for jumping over the conditional
  // calculation.
  gg_create_goto_pair(&tgt->addresses.condover[i].go_to,
                      &tgt->addresses.condover[i].label);

  // Create an unnamed goto/label pair for jumping into the
  // conditional calculation:
  gg_create_goto_pair(&tgt->addresses.condinto[i].go_to,
                      &tgt->addresses.condinto[i].label);

  // Create an unnamed goto/label pair for jumping back from the
  // conditional calculation:
  gg_create_goto_pair(&tgt->addresses.condback[i].go_to,
                      &tgt->addresses.condback[i].label);

  // The next instructions that the parser will give us are the conditional
  // calculation, so the first thing that goes down is the condover:
  gg_append_statement(tgt->addresses.condover[i].go_to);

  // And then, of course, we need to be able to jump back here to actually
  // do the run-time conditional calculations:
  gg_append_statement(tgt->addresses.condinto[i].label);

  tgt->addresses.number_of_conditionals += 1;
  }

void
parser_perform_conditional_end( struct cbl_perform_tgt_t *tgt )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" cbl_perform_tgt_t is at")
    char ach[32];
    sprintf(ach, " %p", (void*)tgt);
    SHOW_PARSE_TEXT(ach);
    SHOW_PARSE_END
    }

  size_t i = tgt->addresses.number_of_conditionals;
  gcc_assert(i);

  // We need to cap off the prior conditional in this chain of conditionals
  gg_append_statement(tgt->addresses.condback[i-1].go_to);
  gg_append_statement(tgt->addresses.condover[i-1].label);
  }

static void
build_N_pairs(tree *go_to, tree *label, size_t N)
  {
  for(size_t i=0; i<N; i++)
    {
    tree a;
    tree b;
    gg_create_goto_pair(&a, &b);
    go_to[i] = a;
    label[i] = b;
    }
  }

static void
perform_outofline_before_until(struct cbl_perform_tgt_t *tgt,
                               bool /*test_before*/,
                               size_t /*N*/,
                               struct cbl_perform_vary_t *varys )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  // This is a PERFORM proc-1 [through proc-2] TEST BEFORE} UNTIL

  /*
      TOP:
                  IF CONDITION 0
                      GOTO EXIT
                  ELSE
                      EXECUTE BODY
                      GOTO TOP
      EXIT:
  */

  create_iline_address_pairs(tgt);

  // Tag the top of the perform
  gg_append_statement(tgt->addresses.top.label);

  // Go do the conditional calculation:

  gg_append_statement(tgt->addresses.condinto[0].go_to);

  // And put down the label so that the conditional calculation knows
  // where to return:
  gg_append_statement(tgt->addresses.condback[0].label);

  char ach[256];
  size_t our_pseudo_label = pseudo_label++;
  sprintf(ach,
          "_proccallb.%ld:",
          our_pseudo_label);
  gg_insert_into_assembler( ach );

  parser_if(varys[0].until);
    {
    // We're done, so leave
    gg_append_statement(tgt->addresses.exit.go_to);
    }
  parser_else();
    {
    // We're not done, so execute the body
    // true means GDB next will fall through
    internal_perform_through(tgt->from(), tgt->to(), true);

    // Jump back to the test:
    gg_append_statement(tgt->addresses.top.go_to );
    }
  parser_fi();

  // Label the bottom of the PERFORM
  gg_append_statement(  tgt->addresses.exit.label );
  sprintf(ach,
          "_procretb.%ld:",
          our_pseudo_label);
  gg_insert_into_assembler( ach );
  }

static void
perform_outofline_after_until(struct cbl_perform_tgt_t *tgt,
                              bool /*test_before*/,
                              size_t /*N*/,
                              struct cbl_perform_vary_t *varys )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  // This is a PERFORM proc-1 [through proc-2] TEST AFTER UNTIL

  /*
      TOP:
                  EXECUTE BODY
                  IF CONDITION 0
                      GOTO EXIT
                  ELSE
                      ADD BY_0 to VARYING_0
                      GOTO TOP
      EXIT:
  */

  char ach[256];
  size_t our_pseudo_label = pseudo_label++;
  sprintf(ach,
          "_proccallb.%ld:",
          our_pseudo_label);
  gg_insert_into_assembler( ach );

  create_iline_address_pairs(tgt);

  // Label the top of the loop
  gg_append_statement(tgt->addresses.top.label);

  // Build the perform:
  // true in the next call means that GDB next will not stop until the entire
  // until loop is finished
  internal_perform_through(tgt->from(), tgt->to(), true);

  // Go recalculate the conditional:
  gg_append_statement( tgt->addresses.condinto[0].go_to);

  // And lay down the label for the come-back from the recalculation:
  gg_append_statement( tgt->addresses.condback[0].label);

  // Assess the conditional
  parser_if(varys[0].until);
  // It's true, so we're done
  gg_append_statement( tgt->addresses.exit.go_to );
  parser_else();
  // It's false, so execute the body again
  gg_append_statement( tgt->addresses.top.go_to );
  parser_fi();
  // Label the bottom of the PERFORM
  gg_append_statement(  tgt->addresses.exit.label );
  sprintf(ach,
          "_procretb.%ld:",
          our_pseudo_label);
  gg_insert_into_assembler( ach );
  }

static void
perform_outofline_testafter_varying(struct cbl_perform_tgt_t *tgt,
                                    bool /*test_before*/,
                                    size_t N,
                                    struct cbl_perform_vary_t *varys )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  // This is a PERFORM proc-1 [THROUGH proc-2] TEST AFTER VARYING

  /*

  [ENTRANCE]
              MOVE FROM_0 TO VARYING_0
  INIT_1:
              MOVE FROM_1 TO VARYING_1
  INIT_2:
              MOVE FROM_2 TO VARYING_2
  . . . . . . . . . . . . . . . . . .
  INIT_N-2:
              MOVE FROM_N-2 TO VARYING_N-2
  INIT_N-1:
              MOVE FROM_N-1 TO VARYING_N-1
              GOTO TOP
  TOP:
              PERFORM PROC-1 [THROUGH PROC-2]
              IF NOT CONDITION_N-1
                  ADD BY_N-1 TO VARYING_N-1
                  GOTO TOP
              IF NOT CONDITION_N-2
                  ADD BY_N-2 TO VARYING_N-2
                  GOTO INIT_N-1
              IF NOT CONDITION_N-3
                  ADD BY_N-3 TO VARYING_N-3
                  GOTO INIT_N-2
  . . . . . . . . . . . . . . . . . .
              IF NOT CONDITION_1
                  ADD BY_1 TO VARYING_1
                  GOTO INIT_2
              IF NOT CONDITION_0
                  ADD BY_0 TO VARYING_0
                  GOTO INIT_1
  EXIT:

  */

  // So, we're going to do that.  But because the initializations
  // and the testing are so nicely loopish, we're going to let
  // the computer create them for us.

  // We are going to need a set of N label pairs.  Actually, we
  // only need N-1; we don't use the zeroth pair.  But the code
  // is cleaner if we just build all N of them.

  char ach[256];
  size_t our_pseudo_label = pseudo_label++;
  sprintf(ach,
          "_proccallb.%ld:",
          our_pseudo_label);
  gg_insert_into_assembler( ach );

  create_iline_address_pairs(tgt);

  tree go_to[MAX_AFTERS];
  tree label[MAX_AFTERS];

  build_N_pairs(go_to, label, N);

  // Build the initialization section:
  for(size_t i=0; i<N; i++)
    {
    gg_append_statement(label[i]);
    parser_move(varys[i].varying, varys[i].from);
    }
  // These next two statements do nothing.  But it'll make sense
  // when we move the logic around to create an inline VARYING
  gg_append_statement(tgt->addresses.top.go_to);
  gg_append_statement(tgt->addresses.top.label);

  // Build the body:
  // true in the next call means that the entire loop will complete
  // even in the face of a GDB next
  internal_perform_through(tgt->from(), tgt->to(), true);

  // Build the test section
  // (The oddball test is because N is a size_t, and can't go negative)
  for(size_t i=N-1; i<N; i--)
    {
    // Jump to the conditional calculation:
    gg_append_statement( tgt->addresses.condinto[i].go_to);

    // And put down the label for the return from that calculation:
    gg_append_statement( tgt->addresses.condback[i].label);

    parser_if( varys[i].until );
    // Condition is true; so we'll fall through
    parser_else();
    // Condition is false, so we increment, and keep going:
    parser_add(varys[i].varying, varys[i].by, varys[i].varying);
    if( i == N-1 )
      {
      gg_append_statement(tgt->addresses.top.go_to);
      }
    else
      {
      gg_append_statement(go_to[i+1]);
      }
    parser_fi();
    }
  // Arriving here means that we all of the conditions were
  // true.  So, we're done.
  sprintf(ach,
          "_procretb.%ld:",
          our_pseudo_label);
  gg_insert_into_assembler( ach );
  }

static void
perform_outofline_before_varying(   struct cbl_perform_tgt_t *tgt,
                                    bool /*test_before*/,
                                    size_t N,
                                    struct cbl_perform_vary_t *varys )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  // This is a PERFORM proc-1 [THROUGH proc-2] TEST BEFORE VARYING

  /*

  ENTRANCE:
              SET ALL VARYING-N to FROM-N
  TEST_0:
              IF CONDITION_0:
                  GOTO EXIT:
  TEST_1:
              IF CONDITION_1:
                  ADD BY_0 TO VARYING_0
                  MOVE FROM_1 TO VARYING_1
                  GOTO TEST_0
  TEST_2:
              IF CONDITION_2:
                  ADD BY_1 TO VARYING_1:
                  MOVE FROM_2 TO VARYING_2
                  GOTO TEST_1:
  TEST_3:
              IF CONDITION_3:
                  ADD BY_2 TO VARYING_2:
                  MOVE FROM_3 TO VARYING_3
                  GOTO TEST_1:
  . . . . . . . . . . . . . . . .
  TEST_N-1:
              IF CONDITION_N-1:
              ADD BY_N-2 TO VARYING_N-2:
              MOVE FROM_N-2 TO VARYING_N-2
              GOTO TEST_N-2
  TOP:
              PERFORM proc-1 [THROUGH proc-2]

              ADD BY_N-1 TO VARYING_N-1:
              GOTO TEST_N-1

  */
  create_iline_address_pairs(tgt);

  tree go_to[MAX_AFTERS];
  tree label[MAX_AFTERS];
  build_N_pairs(go_to, label, N);

  char ach[256];
  size_t our_pseudo_label = pseudo_label++;
  sprintf(ach,
          "_proccallb.%ld:",
          our_pseudo_label);
  gg_insert_into_assembler( ach );

  // Initialize all varying:

  for(size_t i=0; i<N; i++)
    {
    parser_move(varys[i].varying, varys[i].from);
    }

  // Lay down the testing cycle:
  for(size_t i=0; i<N; i++)
    {
    // This is the chain of conditions that gets tested before
    // the statements run.  Each condition gets its own label.
    gg_append_statement(label[i]);

    // go back to the instructions that calculate the conditional
    gg_append_statement(tgt->addresses.condinto[i].go_to);

    // And put down the label that brings us back:
    gg_append_statement(tgt->addresses.condback[i].label);

    // Now we can test the calculated conditional:
    parser_if(varys[i].until);
    // This condition has been met, so we increment the
    // variable to the left, reset ours, and go check the
    // one we just incremented
    if(i == 0)
      {
      // This is the leftmost condition condition, so when it
      // is TRUE, we are done.
      gg_append_statement(  tgt->addresses.exit.go_to );
      }
    else
      {
      // This is one of the conditions to the right of the
      // first one.  So, we augment the VARYING to the
      // left, reset our VARYING, and go test the
      // condition to the left:
      parser_add(varys[i-1].varying, varys[i-1].by, varys[i-1].varying);
      parser_move(varys[i].varying, varys[i].from);
      gg_append_statement( go_to[i-1] );
      }
    parser_else();
    // This condition has not been met.
    if( i == N-1 )
      {
      // ... and this is the rightmost condition
      // This is where we perform the body of the PERFORM.
      gg_append_statement(  tgt->addresses.top.label );

      // Build the body:
      // true in the next call means that GDB NEXT will pass through the
      // entire loop
      internal_perform_through(tgt->from(), tgt->to(), true);

      // And now we augment FROM_N-1 by BY__N-1
      parser_add(varys[N-1].varying, varys[N-1].by, varys[N-1].varying);

      // And we jump back to test that freshly-augmented condition
      gg_append_statement( go_to[N-1] );
      }
    else
      {
      // At this point, a condition that is not the rightmost
      // one has not been met.  We could, in principle, just
      // fall through at this point.  But that makes me nervous.
      // So, I am going to put in what may well be an
      // unnecessary goto:
      gg_append_statement( go_to[i+1] );
      }
    parser_fi();
    }
  // The astute observer will have noted that there is no way
  // for the generated runtime code to reach this point except by jumpint to
  // the EXIT: label.
  // We have, you see, reached the egress:
  gg_append_statement(  tgt->addresses.exit.label );
  sprintf(ach,
          "_procretb.%ld:",
          our_pseudo_label);
  gg_insert_into_assembler( ach );
  }

static void
perform_outofline(  struct cbl_perform_tgt_t *tgt,
                    bool test_before,
                    size_t N,
                    struct cbl_perform_vary_t *varys )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  // This is an out-of-line perform.

  // We need to create the address pairs, because there was no parser_perform_start

  if( N == 1 && !varys[0].varying.field )
    {
    // There is no varys.varying, so this is just a PERFORM proc-1 UNTIL
    if( test_before )
      {
      perform_outofline_before_until(tgt, test_before, N, varys);
      }
    else
      {
      perform_outofline_after_until(tgt, test_before, N, varys);
      }
    }
  else
    {
    // This is a PERFORM proc-1 [through proc-2] VARYING
    if( test_before )
      {
      perform_outofline_before_varying(tgt, test_before, N, varys);
      }
    else
      {
      perform_outofline_testafter_varying(tgt, test_before, N, varys);
      }
    }
  }

static void
perform_inline_until(   struct cbl_perform_tgt_t *tgt,
                        bool test_before,
                        size_t /*N*/,
                        struct cbl_perform_vary_t *varys )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  // This is a PERFORM <inline> [TEST {BEFORE|AFTER}] UNTIL

  /*

              GOTO SETUP
      TOP:    S1
              S2
              EXIT PERFORM -> GOTO EXIT:
              S3
              S4
              EXIT PERFORM CYCLE -> GOTO TEST
              S6
              S7
      TEST:   IF CONDITION
                  GOTO EXIT
              ELSE
                  GOTO TOP
      SETUP:
              IF TEST BEFORE
                  GOTO TEST
              ELSE
                  GOTO TOP
      EXIT:
  */
  gg_set_current_line_number(cobol_location().last_line);

  gg_append_statement(tgt->addresses.test.label);

  // Go to where the conditional is recalculated....
  gg_append_statement(tgt->addresses.condinto[0].go_to);

  // ...and lay down the return address.
  gg_append_statement(tgt->addresses.condback[0].label);

  parser_if( varys[0].until );
  gg_append_statement(  tgt->addresses.exit.go_to );
  parser_else();
  gg_append_statement(  tgt->addresses.top.go_to );
  parser_fi();
  gg_append_statement(  tgt->addresses.setup.label );

  if( test_before )
    {
    gg_append_statement(  tgt->addresses.test.go_to );
    }
  else
    {
    gg_append_statement(  tgt->addresses.top.go_to );
    }
  gg_append_statement(  tgt->addresses.exit.label );
  }

static void
perform_inline_testbefore_varying(  struct cbl_perform_tgt_t *tgt,
                                    bool /*test_before*/,
                                    size_t N,
                                    struct cbl_perform_vary_t *varys )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  // This is a PERFORM proc-1 [THROUGH proc-2] TEST BEFORE VARYING

  /*

              GOTO SETUP
  TOP:
              S1
              S2
              EXIT PERFORM -- GOTO EXIT:
              S3
              S4
              EXIT PERFORM CYCLE -- GOTO TESTA
              S5
              S6
              GOTO AUGMENT_N-1
  SETUP:
              SET ALL VARYING-N to FROM-N
  TEST_0:
              IF CONDITION_0:
                  GOTO EXIT:
  TEST_1:
              IF CONDITION_1:
                  ADD BY_0 TO VARYING_0
                  MOVE FROM_1 TO VARYING_1
                  GOTO TEST_0
  TEST_2:
              IF CONDITION_2:
                  ADD BY_1 TO VARYING_1:
                  MOVE FROM_2 TO VARYING_2
                  GOTO TEST_1:
  TEST_3:
              IF CONDITION_3:
                  ADD BY_2 TO VARYING_2:
                  MOVE FROM_3 TO VARYING_3
                  GOTO TEST_1:
  . . . . . . . . . . . . . . . .
  TEST_N-1:
              IF CONDITION_N-1:
                  ADD BY_N-2 TO VARYING_N-2:
                  MOVE FROM_N-2 TO VARYING_N-2
                  GOTO TEST_N-2

              GOTO TOP
  TESTA:
              ADD BY_N-1 TO VARYING_N-1:
              GOTO TEST_N-1

  */
  tree go_to[MAX_AFTERS];
  tree label[MAX_AFTERS];
  build_N_pairs(go_to, label, N);

  // At this point in the executable, the body of the inline loop has been
  // laid down, so we lay down a GOTO TESTA
  SHOW_PARSE
    {
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT("GOTO TESTA")
    SHOW_PARSE_END
    }
  gg_append_statement(tgt->addresses.testA.go_to);

  // It's now safe to setup the whole extravaganza of UNTIL conditions:
  SHOW_PARSE
    {
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT("LABEL SETUP:")
    SHOW_PARSE_END
    }
  gg_append_statement(tgt->addresses.setup.label);

  // Initialize all varying:
  for(size_t i=0; i<N; i++)
    {
    parser_move(varys[i].varying, varys[i].from);
    }

  gg_set_current_line_number(cobol_location().last_line);

  // Lay down the testing cycle:
  for(size_t i=0; i<N; i++)
    {
    // This is the chain of conditions that gets tested before
    // the statements run.  Each condition gets its own label.
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      char ach[32];
      sprintf(ach, "LABEL [%ld]:", i);
      SHOW_PARSE_TEXT(ach)
      SHOW_PARSE_END
      }
    gg_append_statement(label[i]);

    // Jump to where the conditional is calculated...
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      char ach[32];
      sprintf(ach, "LABEL CONDINTO[%ld]:", i);
      SHOW_PARSE_TEXT(ach)
      SHOW_PARSE_END
      }
    gg_append_statement(tgt->addresses.condinto[i].go_to);

    // ...and lay down the label for the return from there
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      char ach[32];
      sprintf(ach, "LABEL CONDBACK[%ld]:", i);
      SHOW_PARSE_TEXT(ach)
      SHOW_PARSE_END
      }
    gg_append_statement(tgt->addresses.condback[i].label);

    // Test that conditional
    parser_if(varys[i].until);
    // This condition has been met, so we increment the
    // variable to the left, reset ours, and go check the
    // one we just incremented
    if(i == 0)
      {
      // This is the leftmost condition condition, so when it
      // is TRUE, we are done.
      SHOW_PARSE
        {
        SHOW_PARSE_INDENT
        SHOW_PARSE_TEXT("GOTO EXIT")
        SHOW_PARSE_END
        }
      gg_append_statement(  tgt->addresses.exit.go_to );
      }
    else
      {
      // This is one of the conditions to the right of the
      // first one.  So, we augment the VARYING to the
      // left, reset our VARYING, and go test the
      // condition to the left:
      parser_add(varys[i-1].varying, varys[i-1].by, varys[i-1].varying);
      parser_move(varys[i].varying, varys[i].from);
      SHOW_PARSE
        {
        SHOW_PARSE_INDENT
        char ach[32];
        sprintf(ach, "GOTO [%ld]:", i-1);
        SHOW_PARSE_TEXT(ach)
        SHOW_PARSE_END
        }
      gg_append_statement( go_to[i-1] );
      }
    parser_else();
    // This condition has not been met.
    if( i == N-1 )
      {
      // ... and this is the rightmost condition
      // This is where we perform the body of the PERFORM.
      SHOW_PARSE
        {
        SHOW_PARSE_INDENT
        SHOW_PARSE_TEXT("GOTO TOP")
        SHOW_PARSE_END
        }
      gg_append_statement(  tgt->addresses.top.go_to );

      // And now we augment FROM_N-1 by BY__N-1
      SHOW_PARSE
        {
        SHOW_PARSE_INDENT
        SHOW_PARSE_TEXT("LABEL TESTA:")
        SHOW_PARSE_END
        }
      gg_append_statement(tgt->addresses.testA.label);
      parser_add(varys[N-1].varying, varys[N-1].by, varys[N-1].varying);
      // And we jump back to test that freshly-augmented condition
      SHOW_PARSE
        {
        SHOW_PARSE_INDENT
        char ach[32];
        sprintf(ach, "GOTO [%ld]:", N-1);
        SHOW_PARSE_TEXT(ach)
        SHOW_PARSE_END
        }
      gg_append_statement( go_to[N-1] );
      }
    else
      {
      // At this point, a condition that is not the rightmost
      // one has not been met.  We could, in principle, just
      // fall through at this point.  But that makes me nervous.
      // So, I am going to put in what may well be an
      // unnecessary goto:
      SHOW_PARSE
        {
        SHOW_PARSE_INDENT
        char ach[32];
        sprintf(ach, "GOTO [%ld]:", i-1);
        SHOW_PARSE_TEXT(ach)
        SHOW_PARSE_END
        }
      gg_append_statement( go_to[i+1] );
      }
    parser_fi();
    }

  // The astute observer will have noted that there is no way
  // for the generated runtime code to reach this point.
  //
  // We have, you see, reached the egress:
  gg_append_statement(  tgt->addresses.exit.label );
  }

static void
perform_inline_testafter_varying(  struct cbl_perform_tgt_t *tgt,
                                   bool /*test_before*/,
                                   size_t N,
                                   struct cbl_perform_vary_t *varys )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  // This is a PERFORM <inline> TEST AFTER VARYING

  /*

              GOTO SETUP
  TOP:
              S1
              S2
              EXIT PERFORM -- GOTO EXIT:
              S3
              S4
              EXIT PERFORM CYCLE -- GOTO TESTA
              S5
              S6
              GOTO TESTA:

  SETUP:
              MOVE FROM_0 TO VARYING_0
  INIT_1:
              MOVE FROM_1 TO VARYING_1
  INIT_2:
              MOVE FROM_2 TO VARYING_2
  . . . . . . . . . . . . . . . . . .
  INIT_N-2:
              MOVE FROM_N-2 TO VARYING_N-2
  INIT_N-1:
              MOVE FROM_N-1 TO VARYING_N-1
              GOTO TOP
  TESTA:
  TEST_N-1:
              IF NOT CONDITION_N-1
                  ADD BY_N-1 TO VARYING_N-1
                  GOTO TOP
              IF NOT CONDITION_N-2
                  ADD BY_N-2 TO VARYING_N-2
                  GOTO INIT_N-1
              IF NOT CONDITION_N-3
                  ADD BY_N-3 TO VARYING_N-3
                  GOTO INIT_N-2
  . . . . . . . . . . . . . . . . . .
              IF NOT CONDITION_1
                  ADD BY_1 TO VARYING_1
                  GOTO INIT_2
              IF NOT CONDITION_0
                  ADD BY_0 TO VARYING_0
                  GOTO INIT_1
              // At this point, all conditions are true
  EXIT:

  */

  // So, we're going to do that.  But because the initializations
  // and the testing are so nicely loopish, we're going to let
  // the computer create them for us.

  // We are going to need a set of N label pairs.  Actually, we
  // only need N-1; we don't use the zeroth pair.  But the code
  // is cleaner if we just build all N of them.

  tree go_to[MAX_AFTERS];
  tree label[MAX_AFTERS];

  build_N_pairs(go_to, label, N);

  // At this point the code being laid down, the GOTO SETUP was created,
  // followed by the stream of statements.  We terminate it with a
  // goto testa
  gg_append_statement(tgt->addresses.testA.go_to);

  // See the comment in create_iline_address_pairs()
  //gg_force_line_number(tgt->addresses.line_number_of_setup_code-1);

  // That's followed by the SETUP target:
  gg_append_statement(tgt->addresses.setup.label);

  // We now build the initialization section,
  for(size_t i=0; i<N; i++)
    {
    gg_append_statement(label[i]);
    parser_move(varys[i].varying, varys[i].from);
    }

  // Having done all the initialization, we jump back to the start of
  // the list of statements:
  gg_append_statement(tgt->addresses.top.go_to);

  // The list of statements ends with a goto TESTA, and that;s here:
  gg_append_statement(tgt->addresses.testA.label);

  // Build the test section
  // (The oddball test is because N is a size_t, and can't go negative)
  for(size_t i=N-1; i<N; i--)
    {
    // Jump to where the conditional is calculated...
    gg_append_statement(tgt->addresses.condinto[i].go_to);

    // ...and lay down the label to get back from there
    gg_append_statement(tgt->addresses.condback[i].label);

    // Test the newly-recalculated conditional:
    parser_if( varys[i].until );
    // Condition is true; so we'll fall through
    parser_else();
    // Condition is false, so we increment, and keep going:
    parser_add(varys[i].varying, varys[i].by, varys[i].varying);
    if( i == N-1 )
      {
      gg_append_statement(tgt->addresses.top.go_to);
      }
    else
      {
      gg_append_statement(go_to[i+1]);
      }
    parser_fi();
    }

  // Arriving here means that we all of the conditions were
  // true.  So, we're done.
  gg_append_statement(  tgt->addresses.exit.label );
  }

static void
perform_inline_impl( struct cbl_perform_tgt_t *tgt,
                     bool test_before,
                     size_t N,
                     struct cbl_perform_vary_t *varys )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  if( N == 1 && !varys[0].varying.field )
    {
    perform_inline_until(tgt, test_before, N, varys);
    }
  else
    {
    // This is a PERFORM proc-1 [through proc-2] VARYING
    if( !test_before )
      {
      perform_inline_testafter_varying(tgt, test_before, N, varys);
      }
    else
      {
      perform_inline_testbefore_varying(tgt, test_before, N, varys);
      }
    }
  }

void
parser_perform_until(   struct cbl_perform_tgt_t *tgt,
                        bool test_before,
                        size_t N,
                        struct cbl_perform_vary_t *varys )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" cbl_perform_tgt_t is at")
    char ach[32];
    sprintf(ach, " %p", (void*)tgt);
    SHOW_PARSE_TEXT(ach);
    SHOW_PARSE_LABEL(" ", tgt->from())
    if( tgt->to() )
      {
      SHOW_PARSE_LABEL(" THROUGH", tgt->to())
      }
    SHOW_PARSE_END
    }

  gg_set_current_line_number(cobol_location().last_line);
  gg_assign(var_decl_nop, build_int_cst_type(INT, 105));

  if( tgt->from()->type != LblLoop )
    {
    perform_outofline( tgt, test_before, N, varys);
    }
  else
    {
    perform_inline_impl( tgt, test_before, N, varys);
    }
  }

void
parser_perform_inline_times(struct cbl_perform_tgt_t *tgt,
                            struct cbl_refer_t how_many )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_LABEL("", tgt->from());
    SHOW_PARSE_REF(" how_many is ", how_many);
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_FIELD(" into ", how_many.field, " times");
    TRACE1_END
    }

  gcc_assert(tgt);
  cbl_field_t *count = how_many.field;
  if( how_many.is_reference() )
    {
    cbl_internal_error("%s:%d: ignoring subscripts", __func__, __LINE__);
    }
  CHECK_FIELD(count);

  // This has to be on the stack, because performs can be nested
  tree counter       = gg_define_variable(LONG);

  /*
              GOTO SETUP
  TOP:        S1
              EXIT PERFORM  --> GOTO EXIT
              S2
              EXIT PERFORM CYCLE --> GOTO TEST
              S3
  TESTA:
  TEST:       INCREMENT COUNTER
              IF COUNTER LT LIMIT
                  GOTO TOP
              ELSE
                  GOTO EXIT
  SETUP:      INITIALIZE COUNTER
              GOTO TOP
  EXIT:
  */

  // At this point, the GOTO SETUP, the label "TOP:" and the
  // body of the inline perform have been laid down.

  // Tack on the label for TEST and TESTA
  gg_append_statement( tgt->addresses.testA.label );
  gg_append_statement( tgt->addresses.test.label );

  // AT this point, we want to set the line_number to the location of the
  // END-PERFORM statement.
  gg_set_current_line_number(cobol_location().last_line);

  gg_decrement(counter);
  // Do the test:
  IF( counter, gt_op, gg_cast(LONG, integer_zero_node) )
    // We continue
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT("If still counting GOTO TOP")
      SHOW_PARSE_END
      }
    gg_append_statement( tgt->addresses.top.go_to );
  ELSE
    // We are done
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT("If count complete GOTO EXIT")
      SHOW_PARSE_END
      }
    gg_append_statement( tgt->addresses.exit.go_to );
    ENDIF

  // Lay down the SETUP: label
  SHOW_PARSE
    {
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT("LABEL SETUP:")
    SHOW_PARSE_END
    }

  int stash = gg_get_current_line_number();
  gg_set_current_line_number(tgt->addresses.line_number_of_setup_code);
  gg_append_statement( tgt->addresses.setup.label );

  // Get the count:
  SHOW_PARSE
    {
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT("Access the how_many parameter")
    SHOW_PARSE_REF(" ", how_many)
    SHOW_PARSE_END
    }

  get_binary_value( counter,
                    NULL,
                    count,
                    size_t_zero_node);

  SHOW_PARSE
    {
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT("GOTO TOP")
    SHOW_PARSE_END
    }

  // Make sure the initial count is valid:
  IF( counter, gt_op, gg_cast(LONG, integer_zero_node) )
    gg_append_statement( tgt->addresses.top.go_to );
  ELSE
    gg_append_statement( tgt->addresses.exit.go_to );
    ENDIF

  gg_set_current_line_number(stash);

  SHOW_PARSE
    {
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT("LABEL EXIT:")
    SHOW_PARSE_END
    }
  gg_append_statement( tgt->addresses.exit.label );
  }

void
parser_set_conditional88( struct cbl_refer_t refer, bool which_way )
  {
  Analyze();
  struct cbl_field_t *tgt = refer.field;
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD(" ", tgt)
    if( which_way )
      {
      SHOW_PARSE_TEXT(" TRUE");
      }
    else
      {
      SHOW_PARSE_TEXT(" FALSE");
      }
    SHOW_PARSE_END
    }

  CHECK_FIELD(tgt);

  struct cbl_field_t *parent = parent_of(tgt);

  CHECK_FIELD(parent);

  cbl_domain_t *src;

  if( which_way )
    {
    src = tgt->data.domain_of();
    }
  else
    {
    src = tgt->data.false_value_of();
    }

  // We want to set the LEVEL88 target to TRUE (or FALSE), so we need to set
  // the parent of this LEVEL88 to the first element in data.domain (or
  // data.false_value);

  cbl_figconst_t figconst = cbl_figconst_of(src->first.name());

  if( !figconst )
    {
    // We are dealing with an ordinary string.
    static size_t buffer_size = 0;
    static char *buffer = NULL;
    size_t length = src->first.size();
    raw_to_internal(&buffer, &buffer_size, src->first.name(), length);
    move_tree_to_field( parent,
                        gg_string_literal(buffer));
    }
  else
    {
    // This is a figurative constant
    gg_call(VOID,
            "__gg__parser_set_conditional",
            gg_get_address_of(parent->var_decl_node),
            build_int_cst_type(INT, figconst),
            NULL_TREE);
    }
  }

static
void set_user_status(struct cbl_file_t *file)
  {
  // This routine sets the user_status, if any, to the cblc_file_t::status

  // We have to do it this way, because in the case where the file->user_status
  // is in linkage, the memory addresses can end up pointing to the wrong
  // places
  if(file->user_status)
    {
    cbl_field_t *user_status = cbl_field_of(symbol_at(file->user_status));
    gcc_assert( user_status );
    gg_call(VOID,
            "__gg__set_user_status",
            gg_get_address_of(user_status->var_decl_node),
            gg_get_address_of(file->var_decl_node),
            NULL_TREE);
    }
  }

void
parser_file_add(struct cbl_file_t *file)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( file )
      {
      fprintf(stderr, " cbl_file_t: %s", file->name);
      if( file->record_length )
        {
        SHOW_PARSE_TEXT(" file->record_length is %s");
        SHOW_PARSE_TEXT(file->name);
        }
      else
        {
        SHOW_PARSE_TEXT(" file->record_length is ZERO")
        }
      }
    else
      {
      SHOW_PARSE_TEXT( " *file pointer is NULL")
      }
    SHOW_PARSE_END
    }

  if( !file )
    {
    cbl_internal_error("%s(): called with NULL *file", __func__);
    gcc_assert(file);
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("parser_file_add cbl_file_t ")
    TRACE1_TEXT(file->name);
    TRACE1_END
    }

  /*  The FD record can be flagged external.  Without definitive information, I
      am going to assume that the *everything* in the cblc_file_t structure is
      GLOBAL EXTERNAL.  If I have read the specification incorrectly, and it's
      possible for two programs to share a file connector but with, say, two
      different lists of keys, then the cblc_file_t structure will have to
      be changed to have one var_decl node for the common information, and a
      second one for local information.

      */

  gg_variable_scope_t scope;
  if( file->attr & external_e )
    {
    scope = vs_external;
    }
  else
    {
    scope = vs_static;
    }

  char achName[2*sizeof(cbl_name_t)];

  // Use the global structure template declaration to produce the specific
  // structure definition expression:
  strcpy(achName, "_");
  strcat(achName, file->name);
  strcat(achName, "_fc"); // For "File Connector"
  tree new_var_decl = gg_define_variable( cblc_file_type_node,
                                          achName,
                                          scope);

  // We have to convert file->nkey and file->keys to the run-time formats.

  // There can be 0 through N keys, and each of those keys has M fields. Each of
  // the M fields has a "unique" flag, which we pass along as an array of INTs.

  int number_of_key_fields = 0;
  for( size_t i=0; i<file->nkey; i++ )
    {
    number_of_key_fields += file->keys[i].nfield;
    }

  // We create an array of pointers for those fields, adding an additional
  // element for a NULL pointer to indicate the end of the list:
  strcpy(achName, "_");
  strcat(achName, file->name);
  strcat(achName, "_keys");
  tree array_of_keys = gg_define_variable(
                                    build_pointer_type(cblc_field_p_type_node),
                                    achName,
                                    scope);
  gg_assign(array_of_keys,
            gg_cast(build_pointer_type(cblc_field_p_type_node),
                    gg_malloc(build_int_cst_type(SIZE_T,
                                                 (number_of_key_fields+1)
                                                 *int_size_in_bytes(VOID_P)))));

  strcpy(achName, "_");
  strcat(achName, file->name);
  strcat(achName, "_keynum");
  tree key_numbers = gg_define_variable(build_pointer_type(INT),
                                        achName,
                                        scope);
  gg_assign(key_numbers,
            gg_cast(build_pointer_type(INT),
                    gg_malloc(build_int_cst_type(SIZE_T,
                                                 (number_of_key_fields+1)
                                                            *int_size_in_bytes(INT)))));

  strcpy(achName, "_");
  strcat(achName, file->name);
  strcat(achName, "_uniqs");
  tree unique_flags = gg_define_variable( build_pointer_type(INT),
                                          achName,
                                          scope);
  gg_assign(unique_flags,
            gg_cast(build_pointer_type(INT),
                    gg_malloc(build_int_cst_type(SIZE_T,
                                                (number_of_key_fields+1)
                                                            *int_size_in_bytes(INT)))));

  size_t index = 0;
  for( size_t i=0; i<file->nkey; i++ )
    {
    for( size_t j=0; j<file->keys[i].nfield; j++ )
      {
      gg_assign(gg_array_value(array_of_keys, index),
                get_field_p(file->keys[i].fields[j]) );

      gg_assign(gg_array_value(key_numbers, index),
                build_int_cst_type(INT, i+1));

      gg_assign(gg_array_value(unique_flags, index),
                (file->keys[i].unique ? integer_one_node : integer_zero_node));
      index += 1;
      }
    }
  // Terminate the field list with a NULL:
  gg_assign( gg_array_value(array_of_keys, index), gg_cast(cblc_field_p_type_node, null_pointer_node) );

  // Terminate the key-numbers list with a negative 1 as a guardrail:
  gg_assign( gg_array_value(key_numbers, index), integer_minusone_node );

  // Terminate the uniques list with a zero, just to avoid garbage:
  gg_assign( gg_array_value(unique_flags, index), integer_zero_node );

  cbl_file_t::varying_t varies = symbol_file_record_sizes(file);

  gcc_assert(varies.min <= varies.max);

  if(file->access == file_inaccessible_e)
    {
    cbl_internal_error(
          "%s:%d file %s access mode is 'file_inaccessible_e' in %s",
          current_filename.back().c_str(),
          CURRENT_LINE_NUMBER,
          file->name,
          __func__);
    }

  gg_call(VOID,
          "__gg__file_init",
          gg_get_address_of(new_var_decl),
          gg_string_literal(file->name),
          array_of_keys,
          key_numbers,
          unique_flags,
          gg_get_address_of(symbol_file_record(file)->var_decl_node),
          get_field_p(file->password),
          get_field_p(file->user_status),
          get_field_p(file->vsam_status),
          get_field_p(file->record_length),
          get_field_p(file_status_register()),
          build_int_cst_type(SIZE_T, file->reserve),
          build_int_cst_type(INT, (int)file->org),
          build_int_cst_type(INT, (int)file->padding),
          build_int_cst_type(INT, (int)file->access),
          build_int_cst_type(INT, (int)file->optional),
          build_int_cst_type(SIZE_T, varies.min),
          build_int_cst_type(SIZE_T, varies.max),
          NULL_TREE);
  file->var_decl_node = new_var_decl;
  }

static void store_location_stuff(const cbl_name_t statement_name);

void
parser_file_open( size_t nfiles, struct cbl_file_t *files[], int mode_char )
  {
  for(size_t i=0; i<nfiles; i++)
    {
    auto& file = files[i];
    parser_file_open(file, mode_char);
    }
  }

void
parser_file_open( struct cbl_file_t *file, int mode_char )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if(file)
      {
      SHOW_PARSE_TEXT(" ");
      SHOW_PARSE_TEXT(file->name);
      char ach[64];
      sprintf(ach, ", organization is %s", file_org_str(file->org));
      SHOW_PARSE_TEXT(ach);
      }
    else
      {
      SHOW_PARSE_TEXT(" *file is NULL")
      }

    SHOW_PARSE_TEXT(", mode_char: ")
    char ach[2] = "";
    ach[0] = mode_char;
    SHOW_PARSE_TEXT(ach)

    SHOW_PARSE_END
    }

  if( !file )
    {
    cbl_internal_error("parser_file_open called with NULL *file");
    }

  if( !file->var_decl_node )
    {
    cbl_internal_error("parser_file_open for %s called with NULL var_decl_node", file->name);
    }

  if( mode_char == 'a' && (file->access != file_access_seq_e) )
    {
    cbl_internal_error("EXTEND can only be used where %s is ACCESS MODE SEQUENTIAL", file->name);
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("parser_file_open of ")
    TRACE1_TEXT(file->name);
    TRACE1_END
    }

  // The cbl_file_t has a cbl_field_t *filename.  This can be a FldAlphanumeric.
  // The runtime has a (char *)filename, so we need to
  // do a runtime conversion.

  tree psz;   // This is going to be either the name of the file, or the
  // possible run-time environment variable that will contain
  // the name of the file.

  cbl_field_t *field_of_name = symbol_field_forward(file->filename);
  bool quoted_name = false;
  if( field_of_name->type == FldForward )
    {
    // The target of ASSIGN TO was unquoted, but didn't resolve to a
    // cbl_field_t.  This means that the name of the field is an
    // environment variable that will hold the file name
    psz = gg_define_char_star();
    gg_assign(psz, gg_strdup(gg_string_literal(field_of_name->name)));
    }
  else
    {
    // The name is coming from a presumably FldAlphaNumeric variable
    psz = get_string_from(field_of_name);
    quoted_name = true;
    }

  store_location_stuff("OPEN");
  gg_call(VOID,
          "__gg__file_open",
          gg_get_address_of(file->var_decl_node),
          psz,
          build_int_cst_type(INT, mode_char),
          quoted_name ? integer_one_node : integer_zero_node,
          NULL_TREE);
  set_user_status(file);
  }

void
parser_file_close( struct cbl_file_t *file, file_close_how_t how )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if(file)
      {
      SHOW_PARSE_TEXT(" ");
      SHOW_PARSE_TEXT(file->name);
      }
    else
      {
      SHOW_PARSE_TEXT(" *file is NULL ")
      }
    SHOW_PARSE_END
    }

  if( !file )
    {
    cbl_internal_error("parser_file_close called with NULL *file");
    }

  if( !file->var_decl_node )
    {
    cbl_internal_error("parser_file_close for %s called with NULL file->var_decl_node", file->name);
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("parser_file_close of ")
    TRACE1_TEXT(file->name);
    TRACE1_END
    }

  // We are done with the filename.  The library routine will free "filename"
  // memory and set it back to null

  store_location_stuff("CLOSE");
  gg_call(VOID,
          "__gg__file_close",
          gg_get_address_of(file->var_decl_node),
          build_int_cst_type(INT, (int)how),
          NULL_TREE);
  set_user_status(file);
  }

void
parser_file_read( struct cbl_file_t *file,
                  cbl_refer_t /*data_dest*/,
                  int where )
  {
  Analyze();
  // where = -2 means PREVIOUS
  // where = -1 means NEXT
  // where =  1 or more means key N, where N is one-based
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if(file)
      {
      SHOW_PARSE_TEXT(" ");
      SHOW_PARSE_TEXT(file->name);
      }
    else
      {
      SHOW_PARSE_TEXT(" *file is NULL")
      }

    char ach[32];
    sprintf(ach, " where:%d", where);
    SHOW_PARSE_TEXT(ach)
    SHOW_PARSE_END
    }

  if( where == 0 )
    {
    cbl_internal_error("%s:%d file %s 'where' is zero in %s",
          current_filename.back().c_str(),
          CURRENT_LINE_NUMBER,
          file->name,
          __func__);
    where = -1;
    }

  if( !file )
    {
    cbl_internal_error("parser_file_read called with NULL *file");
    }

  if( !file->var_decl_node )
    {
    cbl_internal_error("parser_file_read for %s called with NULL file->var_decl_node", file->name);
    }

  if( !file )
    {
    cbl_internal_error("parser_file_read called with NULL *field");
    }

  if( !file->var_decl_node )
    {
    cbl_internal_error("parser_file_read for %s called with NULL field->var_decl_node", file->name);
    }

  if( file->access == file_access_seq_e && where >= 0)
    {
    cbl_internal_error("%s:%d file %s is RELATIVE/SEQUENTIAL, but 'where' >= 0",
          current_filename.back().c_str(),
          CURRENT_LINE_NUMBER,
          file->name);
    where = -1;
    }

  if( file->access == file_access_rnd_e && where < 0)
    {
    cbl_internal_error("%s:%d file %s is RELATIVE/RANDOM, but 'where' < 0",
          current_filename.back().c_str(),
          CURRENT_LINE_NUMBER,
          file->name);
    where = 1;
    }

  store_location_stuff("READ");
  gg_call(VOID,
          "__gg__file_read",
          gg_get_address_of(file->var_decl_node),
          build_int_cst_type(INT, where),
          NULL_TREE);
  set_user_status(file);

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("from ")
    TRACE1_TEXT(file->name);
    TRACE1_INDENT
    cbl_field_t *our_return_code
                  = cbl_field_of(symbol_at(file_status_register()));
    TRACE1_FIELD("result: ", our_return_code, "");
    TRACE1_END
    }
  }

void
parser_file_write( cbl_file_t *file,
                   cbl_field_t *record_area,
                   bool after,
                   cbl_refer_t &advance,
                   bool sequentially
                 )
  {
  Analyze();

  bool is_random = !(   file->access == file_access_seq_e
                     || file->access == file_inaccessible_e);

  if( (is_random ? 1 : 0) != (sequentially ? 0 : 1) )
    {
    cbl_internal_error("%s:%d file %s 'sequentially' is %d in %s",
          current_filename.back().c_str(),
          CURRENT_LINE_NUMBER,
          file->name,
          sequentially ? 1 : 0,
          __func__);
    }

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if(file)
      {
      SHOW_PARSE_TEXT(" ");
      SHOW_PARSE_TEXT(file->name);
      }
    else
      {
      SHOW_PARSE_TEXT(" *file is NULL")
      }

    if( !advance.field )
      {
      SHOW_PARSE_TEXT(" automatic BEFORE ADVANCING 1 LINE")
      }
    else
      {
      if( after )
        {
        SHOW_PARSE_TEXT(" AFTER")
        }
      else
        {
        SHOW_PARSE_TEXT(" BEFORE")
        }
      SHOW_PARSE_REF(" ADVANCING ", advance);
      SHOW_PARSE_TEXT(" LINE(S)")
      }

    SHOW_PARSE_END
    }

  if( !file )
    {
    cbl_internal_error("%s(): called with NULL *file", __func__);
    }

  if( !file->var_decl_node )
    {
    cbl_internal_error("%s(): for %s called with NULL file->var_decl_node",
                        __func__, file->name);
    }

  if( !file )
    {
    cbl_internal_error("%s(): called with NULL *field", __func__);
    }

  if( !file->var_decl_node )
    {
    cbl_internal_error( "%s(): for %s called with NULL field->var_decl_node",
                        __func__,
                        file->name);
    }

  static tree t_advance = gg_define_variable(INT, "..pfw_advance", vs_file_static);
  if(advance.field)
    {
    static tree value = gg_define_variable(INT, "..pfw_value", vs_file_static);
    get_binary_value( value,
                      NULL,
                      advance.field,
                      refer_offset_source(advance));
    gg_assign(t_advance, gg_cast(INT, value));
    }
  else
    {
    if( file->org == file_line_sequential_e )
      {
      // ISO/IEC_1989-2014 and IBM say the default is AFTER advancing
      // MicroFocus and GnuCOBOL say the default is BEFORE advancing.
      // See the comment where the variable is defined:
      after = auto_advance_is_AFTER_advancing;
      gg_assign(t_advance, integer_one_node);
      }
    else
      {
      // The default for SEQUENTIAL is no vertical motion
      gg_assign(t_advance, integer_minusone_node);
      }
    }

  gcc_assert(record_area);
  if( !record_area )
    {
    record_area = cbl_field_of(symbol_at(file->default_record));
    }

  store_location_stuff("WRITE");
  gg_call(VOID,
          "__gg__file_write",
          gg_get_address_of(file->var_decl_node),
          member(record_area, "data"),
          member(record_area, "capacity"),
          after ? integer_one_node : integer_zero_node,
          t_advance,
          is_random ? integer_one_node : integer_zero_node,
          NULL_TREE);
  set_user_status(file);

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("to ")
    TRACE1_TEXT(file->name);
    TRACE1_INDENT
    if( advance.field )
      {
      TRACE1_INDENT
      if( after )
        {
        TRACE1_TEXT("AFTER")
        }
      else
        {
        TRACE1_TEXT("BEFORE")
        }
      TRACE1_REFER(" ADVANCING ", advance, " LINE(S)");
      }
    TRACE1_INDENT
    cbl_field_t *our_return_code
                  = cbl_field_of(symbol_at(file_status_register()));
    TRACE1_FIELD("result: ", our_return_code, "");
    TRACE1_END
    }
  }

void
parser_file_delete( struct cbl_file_t *file, bool /*sequentially*/ )
  {
  Analyze();
  bool sequentially =    file->access == file_access_seq_e
                      || file->org    == file_sequential_e
                      || file->org    == file_line_sequential_e;
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if(file)
      {
      SHOW_PARSE_TEXT(" ");
      SHOW_PARSE_TEXT(file->name);
      if( sequentially )
        {
        SHOW_PARSE_TEXT(" sequentially")
        }
      else
        {
        SHOW_PARSE_TEXT(" sequentially")
        }
      }
    else
      {
      SHOW_PARSE_TEXT(" *file is NULL")
      }
    SHOW_PARSE_END
    }

  store_location_stuff("DELETE");
  gg_call(VOID,
          "__gg__file_delete",
          gg_get_address_of(file->var_decl_node),
          sequentially ? integer_zero_node : integer_one_node,
          NULL_TREE);
  set_user_status(file);

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("parser_file_delete record ")
    TRACE1_TEXT(file->name);
    TRACE1_END
    }
  }

void
parser_file_rewrite(cbl_file_t *file,
                    cbl_field_t *record_area,
                    bool sequentially )
  {
  Analyze();
  if(    file->org    == file_indexed_e
      && file->access == file_access_seq_e
      && !sequentially )
    {
    cbl_internal_error(
          "%s:%d file %s is INDEXED/SEQUENTIAL, but 'sequentially' is false",
          current_filename.back().c_str(),
          CURRENT_LINE_NUMBER,
          file->name);
    sequentially = true;
    }

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if(file)
      {
      SHOW_PARSE_TEXT(" ");
      SHOW_PARSE_TEXT(file->name);
      }
    else
      {
      SHOW_PARSE_TEXT(" *file is NULL")
      }
    SHOW_PARSE_END
    }

  gcc_assert(record_area);
  if( !record_area )
    {
    record_area = cbl_field_of(symbol_at(file->default_record));
    }

  store_location_stuff("REWRITE");
  gg_call(VOID,
          "__gg__file_rewrite",
          gg_get_address_of(file->var_decl_node),
          member(record_area, "capacity"),
          sequentially ? integer_zero_node : integer_one_node,
          NULL_TREE);
  set_user_status(file);
  }

/*
 * flk is first-last-key.  Similar to parser_file_read, it is a
 * 1-based index, for consistency.  Encoded values:
 *   -1 FIRST
 *   -2 LAST
 *    0 neither
 *   >0 1-based index into cbl_file_t::keys
 */
void
parser_file_start(struct cbl_file_t *file,
                  relop_t op,
                  int flk,
                  cbl_refer_t length_ref )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if(file)
      {
      SHOW_PARSE_TEXT(" ");
      SHOW_PARSE_TEXT(file->name);
      switch(op)
        {
        case lt_op:
          SHOW_PARSE_TEXT(" lt_op")
          break;
        case le_op:
          SHOW_PARSE_TEXT(" le_op")
          break;
        case eq_op:
          SHOW_PARSE_TEXT(" eq_op")
          break;
        case ne_op:
          SHOW_PARSE_TEXT(" ne_op")
          break;
        case ge_op:
          SHOW_PARSE_TEXT(" ge_op")
          break;
        case gt_op:
          SHOW_PARSE_TEXT(" gt_op")
          break;
        }
      char ach[32];
      sprintf(ach, " first-last-key:%d", flk);
      SHOW_PARSE_TEXT(ach)
      SHOW_PARSE_REF(" length:", length_ref);
      }
    else
      {
      SHOW_PARSE_TEXT(" *file is NULL")
      }
    SHOW_PARSE_END
    }

  if(     flk == 0
      &&  (file->org == file_indexed_e || file->org == file_relative_e) )
    {
    flk = 1;
    op = eq_op;
    }

  if(     flk == 0
      &&  (file->org == file_sequential_e) )
    {
    flk = -1;
    }

  static tree length = gg_define_variable(SIZE_T, "..pfs_length", vs_file_static);
  gg_assign(length, size_t_zero_node);

  if( flk > 0 && !length_ref.field )
    {
    // We need a length, and we don't have one.  We have to calculate the length
    // from the lengths of the fields that make up the specified key.

    size_t combined_length = 0;

    gcc_assert(flk <= (int)file->nkey);

    int key_number = flk-1;

    // A key has a number of fields
    for(size_t ifield=0; ifield<file->keys[key_number].nfield; ifield++)
      {
      size_t field_index = file->keys[key_number].fields[ifield];
      cbl_field_t *field = cbl_field_of(symbol_at(field_index));
      combined_length += field->data.capacity;
      }
    gg_assign(length, build_int_cst_type(SIZE_T, combined_length));
    }
  else if( flk > 0 )
    {
    get_binary_value( length,
                      NULL,
                      length_ref.field,
                      refer_offset_dest(length_ref));
    }

  store_location_stuff("START");
  gg_call(VOID,
          "__gg__file_start",
          gg_get_address_of(file->var_decl_node),
          build_int_cst_type(INT, op),
          build_int_cst_type(INT, flk),
          length,
          NULL_TREE );
  set_user_status(file);
  }

static void
inspect_tally(bool backward,
              cbl_refer_t identifier_1,
              unsigned long n_identifier_2,
              cbx_inspect_t<cbl_refer_t>* identifier_2)
  {
  Analyze();
  // This is an INSPECT FORMAT 1
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }

  // Make one pass through the inputs to count up the sizes of the arrays
  // we will be passing to the library routines.  This loop structure simply
  // anticipates the more complex one that follows.

  size_t int_index  = 0;
  size_t pcbl_index = 0;

  // The first integer is the all-important controlling count:
  int_index++;

  // The first refer is for identifier-1
  pcbl_index++;

  for( size_t i=0; i<n_identifier_2; i++)
    {
    // Each identifier-2 has to go into the array:
    pcbl_index++;
    // For each FOR there is a count of the loops after the FOR
    int_index++;
    for(size_t j=0; j<identifier_2[i].nbound; j++)
      {

      // After each identifier-2, there is a cbl_inspect_bound_t value:
      int_index++;
      if( identifier_2[i].opers[j].bound == bound_characters_e)
        {
        // This is a FOR CHARACTERS PHRASE1, so we will need before/after
        // for each:
        pcbl_index++;
        pcbl_index++;
        }
      else
        {
        // This is ALL or LEADING.  Each has some number of identifier-3
        int_index++;
        for(size_t k=0; k<identifier_2[i].opers[j].n_identifier_3; k++)
          {
          // Put identifier-3 into the array:
          pcbl_index++;

          // We need the PHRASE1 for that identifier-3
          pcbl_index++;
          pcbl_index++;
          }
        }
      }
    }

  // We will be passing the library routine an array of size_t, which contains
  // all the integers and cbl_inspect_bound_t values, in a strict sequence so
  // that the library routine can peel them off.

  static tree int_size = gg_define_variable(INT,      "..pit_size", vs_file_static, 0);
  static tree integers = gg_define_variable(SIZE_T_P, "..pit", vs_file_static, null_pointer_node);

  size_t n_integers = int_index;

  IF( build_int_cst_type(INT, n_integers), gt_op, int_size )
    {
    gg_assign(int_size, build_int_cst_type(INT, n_integers));
    gg_assign(integers,
              gg_cast(SIZE_T_P,
                      gg_realloc(integers,
                                 n_integers
                                 * int_size_in_bytes(VOID_P))));
    }
  ELSE
    {
    }
  ENDIF

  size_t n_resolveds = pcbl_index;
  cbl_refer_t *pcbl_refers = (cbl_refer_t *)xmalloc(n_resolveds * sizeof(cbl_refer_t));

  // Now we make a second pass, populating those arrays:
  int_index  = 0;
  pcbl_index = 0;

  // The first integer is the all-important controlling count:
  gg_assign(  gg_array_value(integers, int_index++),
              build_int_cst_type(SIZE_T, n_identifier_2) );

  // The first refer is for identifier-1
  pcbl_refers[pcbl_index++] = identifier_1;

  for( size_t i=0; i<n_identifier_2; i++)
    {
    // Each identifier-2 has to go into the array:
    pcbl_refers[pcbl_index++] = identifier_2[i].tally;
    // For each FOR there is a count of the loops after the FOR
    gg_assign(  gg_array_value(integers, int_index++),
                build_int_cst_type(SIZE_T, identifier_2[i].nbound) );
    for(size_t j=0; j<identifier_2[i].nbound; j++)
      {

      // After each identifier-2, there is a cbl_inspect_bound_t value:
      gg_assign(  gg_array_value(integers, int_index++),
                  build_int_cst_type(SIZE_T, identifier_2[i].opers[j].bound));
      if( identifier_2[i].opers[j].bound == bound_characters_e)
        {
        // This is a FOR CHARACTERS PHRASE1, so we will need before/after
        // for each:
        pcbl_refers[pcbl_index++] = identifier_2[i].opers[j].matches[0].before.identifier_4;
        pcbl_refers[pcbl_index++] = identifier_2[i].opers[j].matches[0].after.identifier_4;
        }
      else
        {
        // This is ALL or LEADING.  Each has some number of identifier-3
        gg_assign(  gg_array_value(integers, int_index++),
                    build_int_cst_type(SIZE_T, identifier_2[i].opers[j].n_identifier_3));
        for(size_t k=0; k<identifier_2[i].opers[j].n_identifier_3; k++)
          {
          // Put identifier-3 into the array:
          pcbl_refers[pcbl_index++] = identifier_2[i].opers[j].matches[k].matching;

          // We need the PHRASE1 for that identifier-3
          pcbl_refers[pcbl_index++] = identifier_2[i].opers[j].matches[k].before.identifier_4;

          pcbl_refers[pcbl_index++] = identifier_2[i].opers[j].matches[k].after.identifier_4;
          }
        }
      }
    }

  //fprintf(stderr, " %ld %ld\n", int_index, n_integers);
  gcc_assert(int_index  == n_integers);
  //fprintf(stderr, " %ld %ld\n", pcbl_index, n_resolveds);
  gcc_assert(pcbl_index == n_resolveds);

  // We have built up an array of integers, and an array of cbl_refer_t.
  build_array_of_treeplets(1, pcbl_index, pcbl_refers);

  // Do the actual call:
  gg_call(VOID,
          "__gg__inspect_format_1",
          backward ? integer_one_node : integer_zero_node,
          integers,
          NULL_TREE);

  // And free up the memory we allocated
  free(pcbl_refers);
  }

static void
inspect_replacing(int backward,
                  cbl_refer_t identifier_1,
                  unsigned long n_ops,
                  cbx_inspect_t<cbl_refer_t>* operations)
  {
  Analyze();
  // This is an INSPECT FORMAT 2
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" ")
    }

  // For REPLACING, unlike TALLY, there can be but one operation
  gcc_assert(n_ops == 1);

  size_t n_id_3 = 0;
  size_t n_id_4 = 0;
  size_t n_id_5 = 0;
  size_t n_all_leading_first = 0;

  // Make one pass through the inputs to count up the sizes of the arrays
  // we will be passing to the library routines:

  for( size_t j=0; j<operations[0].nbound; j++)
    {
    if( operations[0].opers[j].bound == bound_characters_e)
      {
      // This is a FOR CHARACTERS phrase

      // Each will have an identifier-5:
      n_id_5 += 1;

      // Each will have a PHRASE1 comprising BEFORE and AFTER identifier-4 values
      n_id_4 += 2;
      }
    else
      {
      // This is ALL, LEADING, or FIRST.  Each has some number of identifier-3 values:
      // The n_identifier_3 value goes into the integer list, so we'll have
      // to make room for them:
      n_all_leading_first += 1;

      // The n_identifier-3 values will go into the resolved values; we have to
      // leave room for them
      n_id_3 += operations[0].opers[j].n_identifier_3;

      // Likewise identifier-5 values:
      n_id_5 += operations[0].opers[j].n_identifier_3;

      // And each identifier-3 / identifier-5 pair has BEFORE and AFTER phrases:
      n_id_4 += 2 * operations[0].opers[j].n_identifier_3;
      }
    }

  // We will be passing the library routine an array of size_t, which contains
  // all the integers and cbl_inspect_bound_t values, in a strict sequence so
  // that the library routine can peel them off.

  size_t n_integers =   1                     // Room for operations[0].nbound
                        + operations[0].nbound  // Room for all the cbl_inspect_bound_t values
                        + n_all_leading_first;  // Room for all of the n_identifier_3 counts

  static tree int_size = gg_define_variable(INT,      "..pir_size", vs_file_static, 0);
  static tree integers = gg_define_variable(SIZE_T_P, "..pir", vs_file_static, null_pointer_node);

  IF( build_int_cst_type(INT, n_integers), gt_op, int_size )
    {
    gg_assign(int_size, build_int_cst_type(INT, n_integers));
    gg_assign(integers,
              gg_cast(SIZE_T_P,
                      gg_realloc(integers,
                                 n_integers
                                 * int_size_in_bytes(VOID_P))));
    }
  ELSE
    {
    }
  ENDIF

  size_t n_resolveds =      1                 // Room for identifier-1
                            + n_id_3            // Room for the identifier-3 variables
                            + n_id_4            // Room for the identifier-4 variables
                            + n_id_5;           // Room for the identifier-5 variables

  cbl_refer_t *pcbl_refers = (cbl_refer_t *)xmalloc(n_resolveds * sizeof(cbl_refer_t));

  // Now we make a second pass, populating those arrays:
  size_t int_index  = 0;
  size_t pcbl_index = 0;

  // The first integer is the all-important controlling count:
  gg_assign(  gg_array_value(integers, int_index++),
              build_int_cst_type(SIZE_T, operations[0].nbound) );

  // The first refer is for identifier-1
  pcbl_refers[pcbl_index++] = identifier_1;

  for( size_t j=0; j<operations[0].nbound; j++)
    {
    // For each FOR there is a count of the loops after the FOR

    // For each operation, there is a cbl_inspect_bound_t value:
    gg_assign(  gg_array_value(integers, int_index++),
                build_int_cst_type(SIZE_T, operations[0].opers[j].bound));
    if( operations[0].opers[j].bound == bound_characters_e)
      {
      // This is a FOR CHARACTERS PHRASE1

      // Put in the identifier-5 replacement value:
      pcbl_refers[pcbl_index++] = operations[0].opers[j].replaces[0].replacement;

      // Each identifier-5 gets a PHRASE1:
      pcbl_refers[pcbl_index++] = operations[0].opers[j].replaces[0].before.identifier_4;
      pcbl_refers[pcbl_index++] = operations[0].opers[j].replaces[0].after.identifier_4;

      SHOW_PARSE
        {
        if( j )
          {
          SHOW_PARSE_INDENT
          }
        SHOW_PARSE_FIELD("ID-5 ", operations[0].opers[j].replaces[0].replacement.field)
        if(operations[0].opers[j].replaces[0].before.identifier_4.field)
          {
          SHOW_PARSE_FIELD(" before ", operations[0].opers[j].replaces[0].before.identifier_4.field)
          }
        if(operations[0].opers[j].replaces[0].after.identifier_4.field)
          {
          SHOW_PARSE_FIELD(" after ", operations[0].opers[j].replaces[0].after.identifier_4.field)
          }
        SHOW_PARSE_END
        }
      }
    else
      {
      // This is ALL or LEADING.  Each has some number of identifier-3/identifier-5 pairs
      gg_assign(  gg_array_value(integers, int_index++),
                  build_int_cst_type(SIZE_T, operations[0].opers[j].n_identifier_3));
      for(size_t k=0; k<operations[0].opers[j].n_identifier_3; k++)
        {
        // Put identifier-3 into the array:
        pcbl_refers[pcbl_index++] = operations[0].opers[j].replaces[k].matching;

        // Put in the identifier-5 replacement value:
        pcbl_refers[pcbl_index++] = operations[0].opers[j].replaces[k].replacement;

        // We need the PHRASE1 for that identifier-3/identifier-5 pair:
        pcbl_refers[pcbl_index++] = operations[0].opers[j].replaces[k].before.identifier_4;

        pcbl_refers[pcbl_index++] = operations[0].opers[j].replaces[k].after.identifier_4;

        SHOW_PARSE
          {
          if( j || k )
            {
            SHOW_PARSE_INDENT
            }
          SHOW_PARSE_FIELD("ID-3 ", operations[0].opers[j].replaces[k].matching.field)
          SHOW_PARSE_FIELD(" ID-5 ", operations[0].opers[j].replaces[k].replacement.field)
          if( operations[0].opers[j].replaces[k].before.identifier_4.field )
            {
            SHOW_PARSE_FIELD("before ", operations[0].opers[j].replaces[k].before.identifier_4.field)
            }
          if(operations[0].opers[j].replaces[k].after.identifier_4.field)
            {
            SHOW_PARSE_FIELD("after ", operations[0].opers[j].replaces[k].after.identifier_4.field)
            }
          SHOW_PARSE_END
          }
        }
      }
    }

  //fprintf(stderr, "%s(): %ld %ld\n", __func__, int_index, n_integers);
  gcc_assert(int_index  == n_integers);
  //fprintf(stderr, "%s(): %ld %ld\n", __func__, pcbl_index, n_resolveds);
  gcc_assert(pcbl_index == n_resolveds);

  // We have built up an array of integers, and an array of cbl_refer_t.

  for(size_t i=0; i<pcbl_index; i++)
    {
    if( pcbl_refers[i].field && pcbl_refers[i].field->type == FldLiteralN )
      {
      fprintf(stderr, "INSPECT field %s shouldn't be a FldLiteralN\n",
              pcbl_refers[i].field->name);
      gcc_unreachable();
      }
    }

  build_array_of_treeplets(1, pcbl_index, pcbl_refers);

  // Do the actual call:
  gg_call(VOID,
          "__gg__inspect_format_2",
          backward ? integer_one_node : integer_zero_node,
          integers,
          NULL_TREE);
  }

void
parser_inspect(cbl_refer_t identifier_1,
               bool backward,
               unsigned long n_operations,
               cbx_inspect_t<cbl_refer_t>* operations)
  {
  Analyze();
  gcc_assert(n_operations);

  /*  Operating philosophy:  We are going to minimize the amount of
      GENERIC tag creation here at compile time, mainly by eliminating
      the generation of cbl_resolved_t structures that we know
      contain no information. */

  if( operations[0].tally.field )
    {
    // This is a FORMAT 1 "TALLYING"
    inspect_tally(backward, identifier_1, n_operations, operations);
    }
  else
    {
    // This is a FORMAT 2 "REPLACING"
    inspect_replacing(backward, identifier_1, n_operations, operations);
    }
  }

void
parser_inspect_conv(cbl_refer_t input,
                    bool backward,
                    cbl_refer_t original,
                    cbl_refer_t replacement,
                    cbl_inspect_qual_t before,
                    cbl_inspect_qual_t after )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }

  gg_call(CHAR_P,
          "__gg__inspect_format_4",
          backward ? integer_one_node : integer_zero_node,
          input.field ? gg_get_address_of(input.field->var_decl_node)
                      : null_pointer_node,
          refer_offset_source(input),
          refer_size_source(input),
          original.field ? gg_get_address_of(original.field->var_decl_node)
                         : null_pointer_node,
          refer_offset_dest(original),
          refer_size_dest(original),
          replacement.field ? gg_get_address_of(
                              replacement.field->var_decl_node)
                            : null_pointer_node,
          refer_offset_source(replacement),
          replacement.all ? build_int_cst_type(SIZE_T, -1LL)
                          : refer_size_source(replacement),
          after.identifier_4.field ? gg_get_address_of(
                                        after.identifier_4.field->var_decl_node)
                                   : null_pointer_node,
          refer_offset_source(after.identifier_4),
          refer_size_source(after.identifier_4),
          before.identifier_4.field ? gg_get_address_of(
                                       before.identifier_4.field->var_decl_node)
                                    : null_pointer_node,
          refer_offset_source(before.identifier_4),
          refer_size_source(before.identifier_4),
          NULL_TREE
          );
  }

void
parser_module_name( cbl_field_t *tgt, module_type_t type )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  gg_call(VOID,
          "__gg__module_name",
          gg_get_address_of(tgt->var_decl_node),
          build_int_cst_type(INT, type),
          NULL_TREE);
  }

void
parser_intrinsic_numval_c( cbl_field_t *f,
                           cbl_refer_t& input,
                           bool locale,
                           cbl_refer_t& currency,
                           bool anycase,
                           bool test_numval_c ) // true for TEST-NUMVAL-C
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }
  if( locale || anycase )
    {
    gcc_unreachable();
    }
  if( test_numval_c )
    {
    gg_call(INT,
            "__gg__test_numval_c",
            gg_get_address_of(f->var_decl_node),
            gg_get_address_of(input.field->var_decl_node),
            refer_offset_source(input),
            refer_size_source(input),
            currency.field ? gg_get_address_of(currency.field->var_decl_node) : null_pointer_node,
            refer_offset_source(currency),
            refer_size_source(currency),
            NULL_TREE
            );
    }
  else
    {
    gg_call(INT,
            "__gg__numval_c",
            gg_get_address_of(f->var_decl_node),
            gg_get_address_of(input.field->var_decl_node),
            refer_offset_source(input),
            refer_size_source(input),
            currency.field ? gg_get_address_of(currency.field->var_decl_node) : null_pointer_node,
            refer_offset_source(currency),
            refer_size_source(currency),
            NULL_TREE
            );
    }
  }

void
parser_intrinsic_subst( cbl_field_t *f,
                        cbl_refer_t& ref1,
                        size_t argc,
                        cbl_substitute_t * argv )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD(" TO ", f)
    for(size_t i=0; i<argc; i++)
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_FIELD(" ", argv[i].orig.field)
      SHOW_PARSE_FIELD(" ", argv[i].replacement.field)
      }
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }

  store_location_stuff("SUBSTITUTE");
  unsigned char *control_bytes = (unsigned char *)xmalloc(argc * sizeof(unsigned char));
  cbl_refer_t *arg1 = (cbl_refer_t *)xmalloc(argc * sizeof(cbl_refer_t));
  cbl_refer_t *arg2 = (cbl_refer_t *)xmalloc(argc * sizeof(cbl_refer_t));

  for(size_t i=0; i<argc; i++)
    {
    control_bytes[i] =   (argv[i].anycase ?
                                  substitute_anycase_e : 0)
                       + (argv[i].first_last == cbl_substitute_t::subst_first_e ?
                                  substitute_first_e : 0)
                       + (argv[i].first_last == cbl_substitute_t::subst_last_e ?
                                  substitute_last_e : 0);
    arg1[i] = argv[i].orig;
    arg2[i] = argv[i].replacement;
    }

  tree control = gg_array_of_bytes(argc, control_bytes);

  build_array_of_treeplets(1, argc, arg1);
  build_array_of_treeplets(2, argc, arg2);

  gg_call(VOID,
          "__gg__substitute",
          gg_get_address_of(f->var_decl_node),
          gg_get_address_of(ref1.field->var_decl_node),
          refer_offset_source(ref1),
          refer_size_source(ref1),
          build_int_cst_type(SIZE_T, argc),
          control,
          NULL_TREE);

  gg_free(control);

  free(arg2);
  free(arg1);
  free(control_bytes);
  }

void
parser_intrinsic_callv( cbl_field_t *tgt,
                        const char function_name[],
                        size_t nrefs,
                        cbl_refer_t *refs )
  {
  Analyze();
  // We have been given an array of refs[nrefs].  Each ref is a pointer
  // to a cbl_ref_t.  We convert that to a table of pointers to run-time
  // cblc_ref_t structures, and we pass that to the function_name intrinsic
  // function.  It is in charge of conversion to whatever form is needed.

  // We get back a return value, which we convert to tgt based on the
  // intrinsic_return_type

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" of ")
    SHOW_PARSE_TEXT(function_name)
    fprintf(stderr, " with %zd parameters", nrefs);
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("about to call \"")
    TRACE1_TEXT(function_name)
    TRACE1_TEXT("\"")
    for(size_t i=0; i<nrefs; i++)
      {
      TRACE1_INDENT
      gg_fprintf(trace_handle, 1, "parameter %ld: ", build_int_cst_type(SIZE_T, i+1));
      TRACE1_REFER("", refs[i], "")
      }
    }
  store_location_stuff(function_name);
  tree ncount = build_int_cst_type(SIZE_T, nrefs);

  build_array_of_fourplets(1, nrefs, refs);

  gg_call(VOID,
          function_name,
          gg_get_address_of(tgt->var_decl_node),
          ncount,
          NULL_TREE);

  TRACE1
    {
    TRACE1_INDENT
    TRACE1_FIELD("result: ", tgt, "")
    TRACE1_END
    }
  }

void
parser_intrinsic_call_0(cbl_field_t *tgt,
                        const char function_name[])
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" of ")
    SHOW_PARSE_TEXT(function_name)
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("about to call \"")
    TRACE1_TEXT(function_name)
    TRACE1_TEXT("\"")
    }

  if( strcmp(function_name, "__gg__random") == 0 )
    {
    // We have no seed value, so call the "next" routine
    gg_call(VOID,
            "__gg__random_next",
            gg_get_address_of(tgt->var_decl_node),
            NULL_TREE);
    }
  else if( strcmp(function_name, "__gg__when_compiled") == 0 )
    {
    // Pass __gg__when_compiled() the time from right now.
    struct timespec tp;
    clock_gettime(CLOCK_REALTIME, &tp); // time_t tv_sec; long tv_nsec

    store_location_stuff(function_name);
    gg_call(VOID,
            function_name,
            gg_get_address_of(tgt->var_decl_node),
            build_int_cst(SIZE_T, tp.tv_sec),
            build_int_cst(LONG,   tp.tv_nsec),
            NULL_TREE);
    }
  else
    {
    store_location_stuff(function_name);
    gg_call(VOID,
            function_name,
            gg_get_address_of(tgt->var_decl_node),
            NULL_TREE);
    }

  TRACE1
    {
    TRACE1_INDENT
    TRACE1_FIELD("result: ", tgt, "")
    TRACE1_END
    }
  }

void
parser_intrinsic_call_1( cbl_field_t *tgt,
                       const char function_name[],
                       cbl_refer_t& ref1 )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" of ")
    SHOW_PARSE_TEXT(function_name)
    SHOW_PARSE_END
    }

  // There are special cases:
  if( strstr(function_name, "__gg__length") )
    {
    TRACE1
      {
      TRACE1_HEADER
      TRACE1_TEXT("about to call \"")
      TRACE1_TEXT(function_name)
      TRACE1_TEXT("\"")
      TRACE1_INDENT
      TRACE1_REFER("parameter: ", ref1, "")
      }
    size_t upper = ref1.field->occurs.bounds.upper
                                    ? ref1.field->occurs.bounds.upper : 1;
    if( ref1.nsubscript )
      {
      upper = 1;
      }

    if( is_table(ref1.field) && !ref1.nsubscript )
      {
      static tree depending_on = gg_define_variable(LONG, "..pic1_dep");
      gg_get_depending_on_value(depending_on, ref1.field);
      gg_call(VOID,
              "__gg__int128_to_field",
              gg_get_address_of(tgt->var_decl_node),
              gg_cast(INT128,
                      gg_multiply(refer_size_source(ref1),
                                  depending_on)),
              integer_zero_node,
              build_int_cst_type(INT, truncation_e),
              null_pointer_node,
              NULL_TREE );
      }
    else
      {
      if( upper == 1 )
        {
        gg_call(VOID,
                "__gg__int128_to_field",
                gg_get_address_of(tgt->var_decl_node),
                gg_cast(INT128,
                        refer_size_source(ref1)),
                integer_zero_node,
                build_int_cst_type(INT, truncation_e),
                null_pointer_node,
                NULL_TREE );
        }
      else
        {
        gg_call(VOID,
                "__gg__int128_to_field",
                gg_get_address_of(tgt->var_decl_node),
                gg_cast(INT128,
                        gg_multiply(refer_size_source(ref1),
                                    build_int_cst_type(SIZE_T, upper))),
                integer_zero_node,
                build_int_cst_type(INT, truncation_e),
                null_pointer_node,
                NULL_TREE );
        }
      }
    }
  else
    {
    TRACE1
      {
      TRACE1_HEADER
      TRACE1_TEXT("about to call \"")
      TRACE1_TEXT(function_name)
      TRACE1_TEXT("\"")
      TRACE1_INDENT
      TRACE1_REFER("parameter: ", ref1, "")
      }
    gg_call(VOID,
            function_name,
            gg_get_address_of(tgt->var_decl_node),
            gg_get_address_of(ref1.field->var_decl_node),
            refer_offset_source(ref1),
            refer_size_source(ref1),
            NULL_TREE);
    }

  TRACE1
    {
    TRACE1_INDENT
    TRACE1_FIELD("result: ", tgt, "")
    TRACE1_END
    }
  }

void
parser_intrinsic_call_2( cbl_field_t *tgt,
                       const char function_name[],
                       cbl_refer_t& ref1,
                       cbl_refer_t& ref2 )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" of ")
    SHOW_PARSE_TEXT(function_name)
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("about to call \"")
    TRACE1_TEXT(function_name)
    TRACE1_TEXT("\"")
    TRACE1_INDENT
    TRACE1_REFER("parameter 1: ", ref1, "")
    TRACE1_INDENT
    TRACE1_REFER("parameter 2: ", ref2, "")
    }
  store_location_stuff(function_name);
  gg_call(VOID,
          function_name,
          gg_get_address_of(tgt->var_decl_node),
          gg_get_address_of(ref1.field->var_decl_node),
          refer_offset_source(ref1),
          refer_size_source(ref1),
          ref2.field ? gg_get_address_of(ref2.field->var_decl_node) : null_pointer_node,
          refer_offset_source(ref2),
          refer_size_source(ref2),
          NULL_TREE);
  TRACE1
    {
    TRACE1_INDENT
    TRACE1_FIELD("result: ", tgt, "")
    TRACE1_END
    }
  }

void
parser_intrinsic_call_3( cbl_field_t *tgt,
                       const char function_name[],
                       cbl_refer_t& ref1,
                       cbl_refer_t& ref2,
                       cbl_refer_t& ref3 )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" of ")
    SHOW_PARSE_TEXT(function_name)
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("about to call \"")
    TRACE1_TEXT(function_name)
    TRACE1_TEXT("\"")
    TRACE1_INDENT
    TRACE1_REFER("parameter 1: ", ref1, "")
    TRACE1_INDENT
    TRACE1_REFER("parameter 2: ", ref2, "")
    TRACE1_INDENT
    TRACE1_REFER("parameter 3: ", ref3, "")
    }

  store_location_stuff(function_name);

  gg_call(VOID,
          function_name,
          gg_get_address_of(tgt->var_decl_node),
          ref1.field ? gg_get_address_of(ref1.field->var_decl_node) : null_pointer_node,
          refer_offset_source(ref1),
          refer_size_source(ref1),
          ref2.field ? gg_get_address_of(ref2.field->var_decl_node) : null_pointer_node,
          refer_offset_source(ref2),
          refer_size_source(ref2),
          ref3.field ? gg_get_address_of(ref3.field->var_decl_node) : null_pointer_node,
          refer_offset_source(ref3),
          refer_size_source(ref3),
          NULL_TREE);
  TRACE1
    {
    TRACE1_INDENT
    TRACE1_FIELD("result: ", tgt, "")
    TRACE1_END
    }
  }

void
parser_intrinsic_call_4( cbl_field_t *tgt,
                       const char function_name[],
                       cbl_refer_t& ref1,
                       cbl_refer_t& ref2,
                       cbl_refer_t& ref3,
                       cbl_refer_t& ref4 )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" of ")
    SHOW_PARSE_TEXT(function_name)
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("about to call \"")
    TRACE1_TEXT(function_name)
    TRACE1_TEXT("\"")
    TRACE1_INDENT
    TRACE1_REFER("parameter 1: ", ref1, "")
    TRACE1_INDENT
    TRACE1_REFER("parameter 2: ", ref2, "")
    TRACE1_INDENT
    TRACE1_REFER("parameter 3: ", ref3, "")
    TRACE1_INDENT
    TRACE1_REFER("parameter 4: ", ref4, "")
    }
  store_location_stuff(function_name);

  gg_call(VOID,
          function_name,
          gg_get_address_of(tgt->var_decl_node),
          ref1.field ? gg_get_address_of(ref1.field->var_decl_node) : null_pointer_node,
          refer_offset_source(ref1),
          refer_size_source(ref1),
          ref2.field ? gg_get_address_of(ref2.field->var_decl_node) : null_pointer_node,
          refer_offset_source(ref2),
          refer_size_source(ref2),
          ref3.field ? gg_get_address_of(ref3.field->var_decl_node) : null_pointer_node,
          refer_offset_source(ref3),
          refer_size_source(ref3),
          ref4.field ? gg_get_address_of(ref4.field->var_decl_node) : null_pointer_node,
          refer_offset_source(ref4),
          refer_size_source(ref4),
          NULL_TREE);
  TRACE1
    {
    TRACE1_INDENT
    TRACE1_FIELD("result: ", tgt, "")
    TRACE1_END
    }
  }

static void
field_increment(cbl_field_t *fld)
  {
  static tree value   = gg_define_variable(INT128, "..fi_value",   vs_file_static);
  static tree rdigits = gg_define_variable(INT,    "..fi_rdigits", vs_file_static);
  get_binary_value(value, rdigits, fld, size_t_zero_node);
  gg_assign(  value,
              gg_add(value, gg_cast(SIZE_T, integer_one_node)));
  gg_call(VOID,
          "__gg__int128_to_field",
          gg_get_address_of(fld->var_decl_node),
          value,
          rdigits,
          build_int_cst_type(INT, truncation_e),
          null_pointer_node,
          NULL_TREE );
  }

static void
create_lsearch_address_pairs(struct cbl_label_t *name)
  {
  // Create the lsearch structure
  name->structs.lsearch = (cbl_lsearch_t *)xmalloc(sizeof(cbl_lsearch_t));
  cbl_lsearch_t *lsearch = name->structs.lsearch;

  gg_create_goto_pair(&lsearch->addresses.at_exit.go_to,
                      &lsearch->addresses.at_exit.label);

  gg_create_goto_pair(&lsearch->addresses.top.go_to,
                      &lsearch->addresses.top.label);

  gg_create_goto_pair(&lsearch->addresses.bottom.go_to,
                      &lsearch->addresses.bottom.label);
  }

void
parser_next_sentence()
  {
  // Eventually we'll need this.
  }

void
parser_lsearch_start(   cbl_label_t *name,
                        cbl_field_t *table,
                        cbl_field_t *index,
                        cbl_field_t *varying )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( name )
      {
      SHOW_PARSE_TEXT(" ")
      SHOW_PARSE_TEXT(name->name)
      }
    if( table )
      {
      SHOW_PARSE_TEXT(" linear search of ")
      SHOW_PARSE_TEXT(table->name)
      }
    if( index )
      {
      SHOW_PARSE_TEXT(" index is ")
      SHOW_PARSE_TEXT(index->name)
      }
    if( varying )
      {
      SHOW_PARSE_TEXT(" varying ")
      SHOW_PARSE_TEXT(varying->name)
      }
    SHOW_PARSE_END
    }
  // Create the goto/label pairs we are going to be needing:
  create_lsearch_address_pairs(name);
  cbl_lsearch_t *lsearch = name->structs.lsearch;
  lsearch->first_when = true;

  // We need to find the first table element:
  cbl_field_t *current = table;
  while(current)
    {
    if( is_table(current) )
      {
      // Extract the number of elements in that rightmost dimension.
      lsearch->limit = gg_define_variable(LONG);
      gg_get_depending_on_value(lsearch->limit, current);
      break;
      }
    current = parent_of(current);
    }

  // Establish the initial value of our counter:
  lsearch->counter = gg_define_variable(LONG);

  tree value   = gg_define_int128();
  if(varying)
    {
    get_binary_value(value, NULL, varying, size_t_zero_node);
    }
  else if( index )
    {
    get_binary_value(value, NULL, index, size_t_zero_node);
    }
  gg_assign(lsearch->counter, gg_cast(LONG, value));

  // And we need these around, so we can increment them:
  lsearch->index = index;
  lsearch->varying = varying;

  // From here we have to jump to the top of the loop:
  gg_append_statement(lsearch->addresses.top.go_to);

  // The next next instructions will be the body of the at-exit code, so
  // we need a label here so that we can get back to them
  gg_append_statement(lsearch->addresses.at_exit.label);
  }

void
parser_lsearch_conditional(cbl_label_t * name)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( name )
      {
      SHOW_PARSE_TEXT(" ")
      SHOW_PARSE_TEXT(name->name)
      }
    SHOW_PARSE_END
    }
  cbl_lsearch_t *lsearch = name->structs.lsearch;

  if( lsearch->first_when )
    {
    lsearch->first_when = false;
    // We are the first of the WHEN CONDITIONALs, which means we just laid down the final
    // statement of the AT-EXIT imperative statements, which means it's
    // time to leave the SEARCH completely.
    gg_append_statement(lsearch->addresses.bottom.go_to);

    // And that puts us at the top of the loop:
    gg_append_statement(lsearch->addresses.top.label);

    // It is at this point we check to see if we have reached the limit:
    IF( lsearch->counter, gt_op, lsearch->limit )
    // The counter has run out.
    gg_append_statement(lsearch->addresses.at_exit.go_to);
    ELSE
    // Just fall through into the following statements, which are
    // the statements for the conditional for the first WHEN
    ENDIF
    }
  else
    {
    // We are at the end of a WHEN TRUE imperative statement.
    gg_append_statement(lsearch->addresses.bottom.go_to);

    // This is the second or later search_conditional.  Note that the
    // code generated here executes after the first parser_when call, so
    // the jump_over label is ready to be placed.

    // We have to lay down the unnamed label so the prior WHEN can jump past
    // its imperative statements when its condition is not met:
    gg_append_statement(lsearch->jump_over.label);
    }
  // At this point, the parser starts laying down the statements that make
  // up the next conditional.
  }

void
parser_lsearch_when( cbl_label_t *name, cbl_field_t *conditional )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( name )
      {
      SHOW_PARSE_TEXT(" ")
      SHOW_PARSE_TEXT(name->name)
      }
    SHOW_PARSE_END
    }
  cbl_lsearch_t *lsearch = name->structs.lsearch;

  // Arriving here means that all of the conditional statements have been
  // laid down, and we are ready to do the WHEN test:

  parser_if(conditional);
  // We have found what we were looking for.  Fall through to the next
  // set of instructions, which comprise the imperative statement
  // associated with the WHEN condition.
  ELSE
  // The conditional is false. We thus want to skip over the imperative
  // instructions that are about to be laid down.

  // Create an unnamed goto/label pair:
  gg_create_goto_pair(&lsearch->jump_over.go_to,
                      &lsearch->jump_over.label);

  // And lay down the goto.
  gg_append_statement(lsearch->jump_over.go_to);
  ENDIF
  }

void
parser_lsearch_end( cbl_label_t *name )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( name )
      {
      SHOW_PARSE_TEXT(" ")
      SHOW_PARSE_TEXT(name->name)
      }
    SHOW_PARSE_END
    }
  cbl_lsearch_t *lsearch = name->structs.lsearch;

  // Arriving here means we have just laid down the final imperative
  // statements of the final WHEN.  If these statements have been executing,
  // it's now time to leave the SEARCH:
  gg_append_statement(lsearch->addresses.bottom.go_to);

  // It's time to lay down the last jump_over label:
  gg_append_statement(lsearch->jump_over.label);

  // With that in place, we increment stuff:
  gg_assign(lsearch->counter, gg_add(lsearch->counter, gg_cast(LONG, integer_one_node)));
  field_increment(lsearch->index);

  if( lsearch->varying )
    {
    field_increment(lsearch->varying);
    }
  // From here we jump to the top of the loop:
  gg_append_statement(lsearch->addresses.top.go_to);

  // And that means we now lay down the label for the bottom
  gg_append_statement(lsearch->addresses.bottom.label);

  // At this point, we are done with the lsearch structure
  free(lsearch);
  lsearch = NULL;
  }

void
parser_bsearch_start(   cbl_label_t* name,
                        cbl_field_t *table )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( name )
      {
      SHOW_PARSE_TEXT(" ")
      SHOW_PARSE_TEXT(name->name)
      }
    if( table )
      {
      SHOW_PARSE_TEXT(" binary search of ")
      SHOW_PARSE_TEXT(table->name)
      }
    SHOW_PARSE_END
    }

  // We need a cbl_bsearch_t structure:
  name->structs.bsearch = (cbl_bsearch_t *)xmalloc(sizeof(cbl_bsearch_t));
  cbl_bsearch_t *bsearch = name->structs.bsearch;

  // Create the address/label pairs we need
  gg_create_goto_pair(&bsearch->too_small.go_to,
                      &bsearch->too_small.label);

  gg_create_goto_pair(&bsearch->too_big.go_to,
                      &bsearch->too_big.label);

  gg_create_goto_pair(&bsearch->top.go_to,
                      &bsearch->top.label);

  gg_create_goto_pair(&bsearch->first_test.go_to,
                      &bsearch->first_test.label);

  gg_create_goto_pair(&bsearch->bottom.go_to,
                      &bsearch->bottom.label);

  // The logic when we first hit a WHEN needs to be different:
  bsearch->first_when = true;

  // We need to find our table element:
  cbl_field_t *current = table;
  while(current)
    {
    if( is_table(current) )
      {
      break;
      }
    current = parent_of(current);
    }

  // There are a number of things we learn from the field "current"

  // We get the index:
  gcc_assert(current->occurs.indexes.nfield);
  size_t index_index = current->occurs.indexes.fields[0];
  bsearch->index = cbl_field_of( symbol_at(index_index) );
  gcc_assert(bsearch->index);

  // And we get the rightward bound of the number of elements:
  // Not that these are LONGS, not SIZE_T.  If we are searching for something
  // that is smaller than element[0] of the table, then right ends up being
  // -1, so we have to have a signed type.
  bsearch->left   = gg_define_variable(LONG, "_left");
  bsearch->right  = gg_define_variable(LONG, "_right");
  bsearch->middle = gg_define_variable(LONG, "_middle");

  // Assign the left and right values:
  gg_assign(bsearch->left, build_int_cst_type(LONG, 1));
  gg_get_depending_on_value(bsearch->right, current);

  // Create the variable that will take the compare result.
  bsearch->compare_result = gg_define_int();

  // We now jump to the top of the binary testing loop, which comes right
  // after the labels where we handle non-equal cases:
  gg_append_statement(bsearch->top.go_to);

  gg_append_statement(bsearch->too_small.label);
  // Arrive here when the element in the array is smaller than the one we are
  // looking for.  This means that we move bsearch->left to the right:
  gg_assign(bsearch->left, gg_add(bsearch->middle, build_int_cst_type(LONG, 1)));
  gg_append_statement(bsearch->top.go_to);

  gg_append_statement(bsearch->too_big.label);
  // Arrive here when the element in the array is larger than the one we
  // are looking for.  This means we have to move bsearch->right to the left:
  gg_assign(bsearch->right, gg_subtract(bsearch->middle, build_int_cst_type(LONG, 1)));
  // Fall through to TOP:

  gg_append_statement(bsearch->top.label);
  // Arrive here when it is time to check to see if we are done:
  IF( bsearch->left, le_op, bsearch->right )
  // We are not done.  Calculate middle from 'left' and 'right'
  gg_assign(  bsearch->middle,
              gg_add(bsearch->left, bsearch->right) );
  gg_assign(  bsearch->middle,
              gg_divide(bsearch->middle, build_int_cst_type(LONG, 2) ));
  //gg_printf("BSEARCH At the top %ld %ld %ld\n", bsearch->left, bsearch->middle, bsearch->right, NULL_TREE);
  // We need to assign that value to bsearch->index.  It might be possible
  // to assume that bsearch->index is a size_t and just cram the bytes into
  // place at bsearch->index->var_decl_node->data.  But for now we'll
  // be cautious and use the slower, but more assured, method:

  gg_call(VOID,
          "__gg__int128_to_field",
          gg_get_address_of(bsearch->index->var_decl_node),
          gg_cast(INT128, bsearch->middle),
          integer_zero_node,
          build_int_cst_type(INT, truncation_e),
          null_pointer_node,
          NULL_TREE );
  // And with middle/index established, we go do the WHEN clause:
  gg_append_statement(bsearch->first_test.go_to);
  ELSE
  // The search ended without finding anything.  Fall through to the
  // AT-EXIT imperative statements that the parser will lay down right
  // after the call to parser_bsearch_start().
  ENDIF
  }

void
parser_bsearch_conditional( cbl_label_t* name )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( name )
      {
      SHOW_PARSE_TEXT(" ")
      SHOW_PARSE_TEXT(name->name)
      }
    SHOW_PARSE_END
    }
  cbl_bsearch_t *bsearch = name->structs.bsearch;

  if( bsearch->first_when )
    {
    bsearch->first_when = false;
    // The first time we arrive here is after the WHEN part of the SEARCH ALL
    // statement.  We have just finished executing any AT-END statements there
    // might be, so it's time to jump to the bottom:
    gg_append_statement(bsearch->bottom.go_to);

    // Otherwise, the TOP part of the loop just calculated the next middle/index,
    // and we now start processing it

    gg_append_statement(bsearch->first_test.label);
    }
  // The second parser_bsearch_conditional() is caused by the appearance of
  // any subsequent AND clauses.  And, it turns out, we do nothing.

  // The parser lays down the statements that calculate the conditional,
  // and we just wait for parser_bsearch_when()
  }

bool
is_ascending_key(cbl_refer_t key)
  {
  bool retval = true;

  cbl_field_t *family_tree = key.field;
  gcc_assert(family_tree);
  while( family_tree )
    {
    if( family_tree->occurs.nkey )
      {
      break;
      }
    family_tree = parent_of(family_tree);
    }
  gcc_assert(family_tree->occurs.nkey);
  for(size_t i=0; i<family_tree->occurs.nkey; i++)
    {
    for(size_t j=0; j<family_tree->occurs.keys[i].field_list.nfield; j++)
      {
      size_t index_of_field
        = family_tree->occurs.keys[i].field_list.fields[j];
      cbl_field_t *key_field = cbl_field_of(symbol_at(index_of_field));

      if( strcmp( key_field->name,
                  key.field->name ) == 0 )
        {
        retval = family_tree->occurs.keys[i].ascending;
        goto done;
        }
      }
    }

done:
  return retval;
  }

void
parser_bsearch_when(cbl_label_t* name,
                    cbl_refer_t key,
                    cbl_refer_t sarg,
                    bool ascending)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( name )
      {
      SHOW_PARSE_TEXT(" ")
      SHOW_PARSE_TEXT(name->name)
      }
    SHOW_PARSE_END
    }
  cbl_bsearch_t *bsearch = name->structs.bsearch;

  if( ascending )
    {
    cobol_compare(  bsearch->compare_result,
                    key,
                    sarg );
    }
  else
    {
    cobol_compare(  bsearch->compare_result,
                    sarg,
                    key );
    }

  IF( bsearch->compare_result, lt_op, integer_zero_node )
  // The key is smaller than sarg:
  gg_append_statement(bsearch->too_small.go_to);
  ELSE
  ENDIF
  IF( bsearch->compare_result, gt_op, integer_zero_node )
  // The key is larger than sarg:
  gg_append_statement(bsearch->too_big.go_to);
  ELSE
  ENDIF

  // We are at the Goldilocks point.  The clause has been satisfied with
  // an equality, so we will just fall through to the next set of statements
  // that the parser laid down.  They are either the next conditional, or
  // the final imperative statements that get executed when all the
  // clauses are satisfied.
  }

void
parser_bsearch_end( cbl_label_t* name )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( name )
      {
      SHOW_PARSE_TEXT(" ")
      SHOW_PARSE_TEXT(name->name)
      }
    SHOW_PARSE_END
    }
  cbl_bsearch_t *bsearch = name->structs.bsearch;

  // Arriving here means that either the search ran out without finding
  // anything, (see the test up at TOP:), or else we just fell through from
  // the statements that executed after all the WHEN/AFTER clauses were
  // satisifed by equality (meaning there were no jumps to TOO_SMALL: or
  // TOO_LARGE).  In other words: we're done.
  gg_append_statement(bsearch->bottom.label);

  free(bsearch);
  }

tree
gg_array_of_field_pointers( size_t N,
                            cbl_field_t **fields )
  {
  tree retval = gg_define_variable(build_pointer_type(cblc_field_p_type_node));
  gg_assign(retval, gg_cast(build_pointer_type(cblc_field_p_type_node),
                            gg_malloc(build_int_cst_type(SIZE_T,
                                                         N * int_size_in_bytes(VOID_P)))));
  for(size_t i=0; i<N; i++)
    {
    gg_assign(gg_array_value(retval, i), gg_get_address_of(fields[i]->var_decl_node));
    }
  return retval;
  }

static void
push_program_state()
  {
  gg_call(VOID,
          "__gg__push_program_state",
          NULL_TREE);
  }

static void
pop_program_state()
  {
  gg_call(VOID,
          "__gg__pop_program_state",
          NULL_TREE);
  }

void
parser_sort(cbl_refer_t tableref,
            bool duplicates,
            cbl_alphabet_t *alphabet,
            size_t nkeys,
            cbl_key_t *keys )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( tableref.field )
      {
      SHOW_PARSE_REF(" Sort table: ", tableref)
      }
    SHOW_PARSE_END
    }

  cbl_field_t *table = tableref.field;
  gcc_assert(table);
  gcc_assert(table->var_decl_node);
  if( !is_table(table) )
    {
    cbl_internal_error(  "%s(): asked to sort %s, but it's not a table",
            __func__,
            tableref.field->name);
    }
  size_t total_keys = 0;
  for( size_t i=0; i<nkeys; i++ )
    {
    total_keys += keys[i].nfield;
    }
  cbl_field_t **flattened_fields = (cbl_field_t **)xmalloc(total_keys * sizeof(cbl_field_t *));
  size_t *flattened_ascending       = (size_t *)xmalloc(total_keys * sizeof(size_t));

  size_t key_index = 0;
  for( size_t i=0; i<nkeys; i++ )
    {
    for( size_t j=0; j<keys[i].nfield; j++ )
      {
      flattened_fields[key_index]    = keys[i].fields[j];
      flattened_ascending[key_index] = keys[i].ascending ? 1 : 0;
      key_index += 1;
      }
    }

  // Create the array of cbl_field_t pointers for the keys
  tree all_keys = gg_array_of_field_pointers( total_keys, flattened_fields);

  // Create the array of integers that are the flags for ASCENDING:
  tree ascending = gg_array_of_size_t( total_keys, flattened_ascending );

  tree depending_on = gg_define_variable(LONG, "_sort_size");
  gg_get_depending_on_value(depending_on, table);

  if( alphabet )
    {
    push_program_state();
    parser_alphabet_use(*alphabet);
    }
  gg_call(VOID,
          "__gg__sort_table",
          gg_get_address_of(tableref.field->var_decl_node),
          refer_offset_source(tableref),
          gg_cast(SIZE_T, depending_on),
          build_int_cst_type(SIZE_T, key_index),
          all_keys,
          ascending,
          duplicates ? integer_one_node : integer_zero_node,
          NULL_TREE);
  if( alphabet )
    {
    pop_program_state();
    }

  free(flattened_ascending);
  free(flattened_fields);

  gg_free(ascending);
  gg_free(all_keys);
  }

void
parser_file_sort(   cbl_file_t *workfile,
                    bool duplicates,
                    cbl_alphabet_t *alphabet,
                    size_t nkeys,
                    cbl_key_t *keys,
                    size_t ninput,
                    cbl_file_t **inputs,
                    size_t noutput,
                    cbl_file_t **outputs,
                    cbl_perform_tgt_t *in_proc,
                    cbl_perform_tgt_t *out_proc )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  // This is the implementation of SORT FORMAT 1

  // It proceeds in three phases.

  // The first phase is absorbing the input and writing it out to the workfile:

  parser_file_open(workfile, 'w');
  IF( member(workfile, "io_status"), ge_op, build_int_cst_type(INT, FsEofSeq) )
    {
    gg_printf("Couldn't open the SORT workfile for writing\n", NULL_TREE);
    gg_exit(integer_one_node);
    }
  ELSE
    ENDIF

  if( in_proc && !ninput )
    {
    // We are getting our inputs from an input procedure
    parser_perform(in_proc, NULL);
    }
  else if( ninput && !in_proc )
    {
    // ninput means there was a USING clause, specifying input files.

    // We are going to transfer the input file[s] to the workfile.  The
    // transfer will be done so that any newlines in a LINE SEQUENTIAL file
    // are skipped, and so that any records that are too long, or too short,
    // are all normalized to the format of the SD record.
    for(size_t i=0; i<ninput; i++)
      {
      parser_file_open(inputs[i], 'r');
      IF( member(workfile, "io_status"), ge_op, build_int_cst_type(INT, FsEofSeq) )
        {
        gg_printf("Couldn't open the SORT USING file for input\n", NULL_TREE);
        gg_exit(integer_one_node);
        }
      ELSE
        ENDIF

      gg_call(VOID,
              "__gg__file_sort_ff_input",
              gg_get_address_of(workfile-> var_decl_node),
              gg_get_address_of(inputs[i]->var_decl_node),
              NULL_TREE);
      parser_file_close(inputs[i]);
      }
    }
  else
    {
    // Having both or neither violates SORT syntax
    cbl_internal_error("%s(): syntax error -- both (or neither) USING "
          "and input-proc are specified",
          __func__);
    }
  parser_file_close(workfile);

  // At this point, we have workfile of unsorted data.  We have a library
  // routine that sorts the workfile.  It needs the keys:

  // The following is a tad more complex than it needs to be.  It's a partial
  // clone of the code for handling multiple keys, each of which can have
  // multiple fields.

  size_t total_keys = 0;
  for( size_t i=0; i<nkeys; i++ )
    {
    total_keys += keys[i].nfield;
    }
  cbl_field_t **flattened_fields = (cbl_field_t **)xmalloc(total_keys * sizeof(cbl_field_t *));
  size_t *flattened_ascending    = (size_t *)      xmalloc(total_keys * sizeof(size_t));

  size_t key_index = 0;
  for( size_t i=0; i<nkeys; i++ )
    {
    for( size_t j=0; j<keys[i].nfield; j++ )
      {
      flattened_fields[key_index]    = keys[i].fields[j];
      flattened_ascending[key_index] = keys[i].ascending ? 1 : 0;
      key_index += 1;
      }
    }

  // Create the array of cbl_field_t pointers for the keys
  tree all_keys = gg_array_of_field_pointers( total_keys, flattened_fields);

  // Create the array of integers that are the flags for ASCENDING:
  tree ascending = gg_array_of_size_t( total_keys, flattened_ascending );

  // We need to open the workfile for the sorting routine:
  parser_file_open(workfile, 'r');
  IF( member(workfile, "io_status"),
      ge_op,
      build_int_cst(INT, FhNotOkay) )
    {
    rt_error("Couldn't open workfile for sorting in parser_file_sort\n");
    }
  ELSE
  ENDIF
  if( alphabet )
    {
    push_program_state();
    parser_alphabet_use(*alphabet);
    }
  gg_call(VOID,
          "__gg__sort_workfile",
          gg_get_address_of(workfile->var_decl_node),
          build_int_cst_type(SIZE_T, key_index),
          all_keys,
          ascending,
          duplicates ? integer_one_node : integer_zero_node,
          NULL_TREE);
  if( alphabet )
    {
    pop_program_state();
    }
  parser_file_close(workfile);

  free(flattened_ascending);
  free(flattened_fields);
  gg_free(ascending);
  gg_free(all_keys);

  // The workfile is sorted.  We move to Phase 3 -- transferring the workfile
  // to the output.

  if( noutput && !out_proc)
    {
    // We have a GIVING phrase:
    for(size_t i=0; i<noutput; i++)
      {
      // Open WORKFILE again to position it at the beginning
      parser_file_open(workfile, 'r');
      IF( member(workfile, "io_status"),
          ge_op,
          build_int_cst(INT, FhNotOkay) )
        {
        rt_error("Couldn't open workfile for transfer to GIVING"
                  "in parser_file_sort");
        }
      ELSE
      ENDIF
      parser_file_open(outputs[i], 'w');
      IF( member(outputs[i], "io_status"),
          ge_op,
          build_int_cst(INT, FhNotOkay) )
        {
        rt_error("Couldn't open GIVING file in parser_file_sort");
        }
      ELSE
      ENDIF
      gg_call(VOID,
              "__gg__file_sort_ff_output",
              gg_get_address_of(outputs[i]->var_decl_node),
              gg_get_address_of(workfile->var_decl_node),
              NULL_TREE);
      parser_file_close(outputs[i]);
      parser_file_close(workfile);
      }
    }
  else if (!noutput && out_proc)
    {
    // We are going to transfer the workfile to the output procedures.
    parser_file_open(workfile,'r');
    IF( member(workfile, "io_status"),
        ge_op,
        build_int_cst(INT, FhNotOkay) )
      {
      rt_error("Couldn't open workfile for stage-three "
                "output in parser_file_sort");
      }
    ELSE
      {
      parser_perform(out_proc, NULL);
      parser_file_close(workfile);
      }
    ENDIF
    }
  else
    {
    cbl_internal_error("%s(): syntax error -- both (or neither) GIVING "
          "and output-proc are specified", __func__);
    }
  }

void
parser_release( cbl_field_t *record_area )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }

  // When this routine is called, it writes the contents of 'record_area' to the
  // workfile specified by the cbl_file_t parent of record_area:

  cbl_file_t *workfile = symbol_record_file(record_area);

  gg_call(VOID,
          "__gg__file_write",
          gg_get_address_of( workfile->var_decl_node),
          member(record_area, "data"),
          member(record_area, "capacity"),
          integer_zero_node,
          integer_minusone_node,
          integer_zero_node,
          NULL_TREE); // non-random
  set_user_status(workfile);
  }

void
parser_return_start( cbl_file_t *workfile, cbl_refer_t into )
  {
  Analyze();
  // This function helps implement the COBOL RETURN statement, which is used
  // in SORT and MERGE to "return" data from an intermediate sort/merge file
  // to SORT/MERGE output procedure.

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }

  // We assume that workfile is open.

  workfile->addresses = (cbl_sortreturn_t *)xmalloc(sizeof(cbl_sortreturn_t));
  gg_create_goto_pair(&workfile->addresses->at_end.go_to,
                      &workfile->addresses->at_end.label);
  gg_create_goto_pair(&workfile->addresses->not_at_end.go_to,
                      &workfile->addresses->not_at_end.label);
  gg_create_goto_pair(&workfile->addresses->bottom.go_to,
                      &workfile->addresses->bottom.label);

  // Read the data from workfile into the SD record position:
  cbl_field_t *data_location = symbol_file_record(workfile);
  parser_file_read(workfile, data_location, -1 );

  // And jump to either at_end or not_at_end, depending:
  IF( member(workfile, "io_status"), lt_op, build_int_cst(INT, FsEofSeq) )
    {
    // The read was successful.  We move the result into place
    if( into.field )
      {
      cbl_field_t *record_area =
                             cbl_field_of(symbol_at(workfile->default_record));
      parser_move(into, record_area, truncation_e);
      }
    // And having moved -- or not -- the record, jump to the not-at-end
    // imperative
    gg_append_statement(workfile->addresses->not_at_end.go_to);
    }
  ELSE
    ENDIF

  IF( member(workfile, "io_status"), lt_op, build_int_cst(INT, FsKeySeq) )
    {
    // The read didn't succeed because of an end-of-file condition
    gg_append_statement(workfile->addresses->at_end.go_to);
    }
  ELSE
    ENDIF

  // Arriving here means some kind of error condition.  So, we don't do the
  // move, and we jump to the end of the statement
  gg_append_statement(workfile->addresses->bottom.go_to);
  }

void
parser_return_atend( cbl_file_t *workfile )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }

  // There might or might not be an at_end clause, and it might, or might
  // not, appear after a not_at_end clause.  If we are appearing after
  // a not_at_end clause, we need to finish that clause with a jump to the
  // bottom of the logic:
  if( !workfile->addresses->not_at_end.label )
    {
    // We have been preceded by a not_at_end label.  So, we need to
    // put in a jump to end those statements:
    gg_append_statement(workfile->addresses->bottom.go_to);
    }
  // And now we place the at_end label:
  gg_append_statement(workfile->addresses->at_end.label);

  // And having placed it, NULL it out
  workfile->addresses->at_end.label = NULL;

  // The imperative statements of the NOT AT END clause will follow
  }

void
parser_return_notatend( cbl_file_t *workfile )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  // There might or might not be a not_at_end clause, and it might, or might
  // not, appear after a at_end clause.  If we are appearing after
  // a at_end clause, we need to finish that clause with a jump to the
  // bottom of the logic:
  if( !workfile->addresses->at_end.label )
    {
    // We have been preceded by an at_end label.  So, we need to
    // put in a jump to end those statements:
    gg_append_statement(workfile->addresses->bottom.go_to);
    }
  // And now we place the not_at_end label:
  gg_append_statement(workfile->addresses->not_at_end.label);

  // And having placed it, NULL it out
  workfile->addresses->not_at_end.label = NULL;

  // The imperative statements of the AT END clause will follow
  }

void
parser_return_finish( cbl_file_t *workfile )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }

  // If we are preceded by either an at_end or not_at_end clause, we need
  // to end those statements with a jump to the bottom:
  if( !workfile->addresses->at_end.label || !workfile->addresses->not_at_end.label)
    {
    gg_append_statement(workfile->addresses->bottom.go_to);
    }

  // We need to place labels for clauses that weren't explicitly expressed
  // in the COBOL source code.  (Both were explicit targets of goto statements
  // back in parser_return_start, so we need to place them here if they
  // weren't placed elsewhere)
  if( workfile->addresses->at_end.label )
    {
    gg_append_statement(workfile->addresses->at_end.label);
    }
  if( workfile->addresses->not_at_end.label )
    {
    gg_append_statement(workfile->addresses->not_at_end.label);
    }
  // And that brings us to the bottom:
  gg_append_statement(workfile->addresses->bottom.label);

  free(workfile->addresses);
  }

static tree
gg_array_of_file_pointers(  size_t N,
                            cbl_file_t **files )
  {
  tree retval = gg_define_variable(build_pointer_type(cblc_file_p_type_node));
  gg_assign(retval, gg_cast(  build_pointer_type(cblc_file_p_type_node),
                              gg_malloc(  build_int_cst_type(SIZE_T,
                                                             N * int_size_in_bytes(VOID_P)))));
  for(size_t i=0; i<N; i++)
    {
    gg_assign(gg_array_value(retval, i), gg_get_address_of(files[i]->var_decl_node));
    }
  return retval;
  }

void
parser_file_merge(  cbl_file_t *workfile,
                    cbl_alphabet_t *alphabet,
                    size_t nkeys,
                    cbl_key_t *keys,
                    size_t ninputs,
                    cbl_file_t **inputs,
                    size_t noutputs,
                    cbl_file_t **outputs,
                    cbl_perform_tgt_t *out_proc )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }

  // Our default file organization is LINE SEQUENTIAL, which spectacularly does
  // *not* work for a SORT workfile.
  if( workfile->org == file_line_sequential_e )
    {
    workfile->org = file_sequential_e;
    gg_assign(  member(workfile->var_decl_node, "org"),
                build_int_cst_type(INT, file_sequential_e));
    }

  size_t total_keys = 0;
  for( size_t i=0; i<nkeys; i++ )
    {
    total_keys += keys[i].nfield;
    }
  cbl_field_t **flattened_fields
            = (cbl_field_t **)xmalloc(total_keys * sizeof(cbl_field_t *));
  size_t *flattened_ascending
            = (size_t *)xmalloc(total_keys * sizeof(size_t));

  size_t key_index = 0;
  for( size_t i=0; i<nkeys; i++ )
    {
    for( size_t j=0; j<keys[i].nfield; j++ )
      {
      flattened_fields[key_index]    = keys[i].fields[j];
      flattened_ascending[key_index] = keys[i].ascending ? 1 : 0;
      key_index += 1;
      }
    }

  // Create the array of cbl_field_t pointers for the keys
  tree all_keys = gg_array_of_field_pointers(total_keys, flattened_fields);

  // Create the array of integers that are the flags for ASCENDING:
  tree ascending = gg_array_of_size_t(total_keys, flattened_ascending);

  tree all_files = gg_array_of_file_pointers(ninputs, inputs);

  // We need to open all of the input files and the workfile.  It's easiest to
  // do that here, rather than in the libgcobol, because of the possibility that
  // the filename is in a variable or an environment variable, rather than a
  // literal. This is handled by parser_file_open() in a way that would be
  // inconvenient in __gg__file_open

  parser_file_open(workfile, 'w');
  IF( member(workfile, "io_status"),
      ge_op,
      build_int_cst_type(INT, FhNotOkay) )
    {
    rt_error("Couldn't open workfile for stage-one "
              "writing in parser_file_merge");
    }
  ELSE
    ENDIF

  for(size_t i=0; i<ninputs; i++)
    {
    if( process_this_exception(ec_sort_merge_file_open_e) )
      {
      IF( member(inputs[i], "file_pointer"), ne_op, null_pointer_node )
        {
        if( enabled_exceptions.match(ec_sort_merge_file_open_e) )
          {
          set_exception_code(ec_sort_merge_file_open_e);
          }
        else
          {
          rt_error("FILE MERGE file not open");
          }
        }
      ELSE
        ENDIF
      }

    parser_file_open(inputs[i], 'r');
    IF( member(inputs[i], "io_status"),
        ge_op,
        build_int_cst_type(INT, FhNotOkay) )
      {
      char ach[128];
      sprintf(ach,
              "Couldn't open %s for stage-one reading in parser_file_merge",
              inputs[i]->name);
      rt_error(ach);
      }
    ELSE
      ENDIF
    }

  cbl_field_t *sd_record = symbol_file_record(workfile);
  if( alphabet )
    {
    push_program_state();
    parser_alphabet_use(*alphabet);
    }
  gg_call(VOID,
          "__gg__merge_files",
          gg_get_address_of(workfile->var_decl_node),
          build_int_cst_type(SIZE_T, nkeys),
          all_keys,
          ascending,
          build_int_cst_type(SIZE_T, ninputs),
          all_files,
          NULL_TREE);
  if( alphabet )
    {
    pop_program_state();
    }

  free(flattened_ascending);
  free(flattened_fields);
  gg_free(ascending);
  gg_free(all_keys);

  parser_file_close(workfile);
  for(size_t i=0; i<ninputs; i++)
    {
    parser_file_close(inputs[i]);
    }

  // The merged workfile has been created.
  if( noutputs && !out_proc)
    {
    // We are going to transfer the workfile to the output files.
    for(size_t i=0; i<noutputs; i++)
      {
      if( process_this_exception(ec_sort_merge_file_open_e) )
        {
        IF( member(outputs[i], "file_pointer"), ne_op, null_pointer_node )
          {
          if( enabled_exceptions.match(ec_sort_merge_file_open_e) )
            {
            set_exception_code(ec_sort_merge_file_open_e);
            }
          else
            {
            rt_error("FILE MERGE file not open");
            }
          }
        ELSE
          ENDIF
        }
      // We keep reopening the workfile as a convenient way to make sure it is
      // positioned at the beginning.
      parser_file_open(workfile,'r');
      IF( member(workfile, "io_status"),
          ge_op,
          build_int_cst_type(INT, FhNotOkay) )
        {
        rt_error("Couldn't open workfile for stage-three "
                  "reading in parser_file_merge\n");
        }
      ELSE
        ENDIF

      parser_file_open(outputs[i], 'w');
      IF( member(outputs[i], "io_status"),
          ge_op,
          build_int_cst_type(INT, FhNotOkay) )
        {
        rt_error("Couldn't open an output file in parser_file_merge");
        }
      ELSE
        ENDIF
      gg_call(VOID,
              "__gg__file_sort_ff_output",
              gg_get_address_of(outputs[i]->var_decl_node),
              gg_get_address_of(workfile->  var_decl_node),
              gg_get_address_of(sd_record-> var_decl_node),
              NULL_TREE);
      parser_file_close(outputs[i]);
      parser_file_close(workfile);
      }
    }
  else if (!noutputs && out_proc)
    {
    // We are going to transfer the workfile to the output procedures.
    parser_file_open(workfile,'r');
    IF( member(workfile, "io_status"),
        ge_op,
        build_int_cst_type(INT, FhNotOkay) )
      {
      rt_error("Couldn't open workfile for"
                         " stage-three output in parser_file_merge");
      }
    ELSE
      ENDIF
    parser_perform(out_proc, NULL);
    parser_file_close(workfile);
    }
  else
    {
    cbl_internal_error("%s(): syntax error -- both (or neither) "
          "files and output-proc are specified", __func__);
    }
  }

void
parser_string_overflow( cbl_label_t *name )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  /*
   *  parser_string_overflow is called 0-2 times before the associated
   *  parser_string.
   */

  name->structs.unstring
    = (cbl_unstring_t *)xmalloc(sizeof(struct cbl_unstring_t) );

  // Set up the address pairs for this clause
  gg_create_goto_pair(&name->structs.unstring->over.go_to,
                      &name->structs.unstring->over.label);
  gg_create_goto_pair(&name->structs.unstring->into.go_to,
                      &name->structs.unstring->into.label);
  gg_create_goto_pair(&name->structs.unstring->bottom.go_to,
                      &name->structs.unstring->bottom.label);

  // Jump over the [NOT] ON OVERFLOW code that is about to be laid down
  gg_append_statement( name->structs.unstring->over.go_to );

  // Create the label that allows the following code to be executed at
  // the appropriate time.
  gg_append_statement( name->structs.unstring->into.label );
  }

void
parser_string_overflow_end( cbl_label_t *name )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  gg_append_statement( name->structs.unstring->bottom.go_to );
  }

void
parser_unstring(cbl_refer_t src,
                size_t ndelimited,
                cbl_refer_t *delimiteds,
                size_t noutputs,
                cbl_refer_t *outputs,
                cbl_refer_t *delimiters,
                cbl_refer_t *counts,
                cbl_refer_t pointer,
                cbl_refer_t tally,
                cbl_label_t *overflow,
                cbl_label_t *not_overflow )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  if( overflow )
    {
    gg_append_statement(overflow->structs.unstring->over.label);
    }
  if( not_overflow )
    {
    gg_append_statement(not_overflow->structs.unstring->over.label);
    }

  cbl_refer_t *delims = (cbl_refer_t *)xmalloc(ndelimited * sizeof(cbl_refer_t));
  char *alls = (char *)xmalloc(ndelimited+1);

  for(size_t i=0; i<ndelimited; i++)
    {
    delims[i] = delimiteds[i];
    alls[i] = delimiteds[i].all ? '1' : '0' ;
    }
  alls[ndelimited] = '\0';

  tree t_alls         = build_string_literal(ndelimited+1, alls);

  build_array_of_treeplets(1, ndelimited, delims);
  build_array_of_treeplets(2, noutputs,   outputs);
  build_array_of_treeplets(3, noutputs,   delimiters);
  build_array_of_treeplets(4, noutputs,   counts);

  tree t_overflow = gg_define_int();
  gg_assign(t_overflow,
            gg_call_expr( INT,
                          "__gg__unstring",
                          gg_get_address_of(src.field->var_decl_node),
                          refer_offset_source(src),
                          refer_size_source(src),
                          build_int_cst_type(SIZE_T, ndelimited),
                          t_alls,
                          build_int_cst_type(SIZE_T, noutputs),
                          pointer.field ? gg_get_address_of(pointer.field->var_decl_node) : null_pointer_node,
                          refer_offset_dest(pointer),
                          refer_size_dest(pointer),
                          tally.field ? gg_get_address_of(tally.field->var_decl_node) : null_pointer_node,
                          refer_offset_dest(tally),
                          refer_size_dest(tally),
                          NULL_TREE)
                          );
  free(alls);
  free(delims);

  if( overflow )
    {
    // We have an ON OVERFLOW clause:
    IF( t_overflow, ne_op, integer_zero_node )
    // And we have an overflow condition
    gg_append_statement( overflow->structs.unstring->into.go_to );
    ELSE
    ENDIF
    }

  if( not_overflow )
    {
    // We have a NOT ON OVERFLOW clause:
    IF( t_overflow, eq_op, integer_zero_node )
    // And there isn't an overflow condition:
    gg_append_statement( not_overflow->structs.unstring->into.go_to );
    ELSE
    ENDIF
    }

  if( overflow )
    {
    gg_append_statement( overflow->structs.unstring->bottom.label );
    free( overflow->structs.unstring );
    }

  if( not_overflow )
    {
    gg_append_statement( not_overflow->structs.unstring->bottom.label );
    free( not_overflow->structs.unstring );
    }
  }

void
parser_string(  cbl_refer_t tgt,
                cbl_refer_t pointer,
                size_t nsource,
                cbl_string_src_t *sources,
                cbl_label_t *overflow,
                cbl_label_t *not_overflow )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  if( overflow )
    {
    gg_append_statement(overflow->structs.unstring->over.label);
    }
  if( not_overflow )
    {
    gg_append_statement(not_overflow->structs.unstring->over.label);
    }

  // We need an array of nsource+1 integers:
  size_t *integers = (size_t *)xmalloc((nsource+1)*sizeof(size_t));

  // Count up how many treeplets we are going to need:
  size_t cblc_count = 2;  // tgt and pointer
  for(size_t i=0; i<nsource; i++)
    {
    cblc_count += 1 + sources[i].ninput; // 1 for identifier_2 + ninput identifier_1 values;
    }

  cbl_refer_t *refers = (cbl_refer_t *)xmalloc(cblc_count * sizeof(cbl_refer_t));

  size_t index_int = 0;
  size_t index_cblc = 0;

  integers[index_int++] = nsource;

  refers[index_cblc++] = tgt;
  refers[index_cblc++] = pointer;

  for(size_t i=0; i<nsource; i++)
    {
    integers[index_int++] = sources[i].ninput;
    refers[index_cblc++] = sources[i].delimited_by;
    for(size_t j=0; j<sources[i].ninput; j++)
      {
      refers[index_cblc++] = sources[i].inputs[j];
      }
    }

  gcc_assert(index_int == nsource+1);
  gcc_assert(index_cblc == cblc_count);

  tree pintegers = build_array_of_size_t( index_int, integers);

  build_array_of_treeplets(1, index_cblc, refers);

  tree t_overflow = gg_define_int();
  gg_assign(t_overflow, gg_call_expr( INT,
                                      "__gg__string",
                                      pintegers,
                                      NULL_TREE));
  gg_free(pintegers);

  free(integers);
  free(refers);

  if( overflow )
    {
    // We have an ON OVERFLOW clause:
    IF( t_overflow, ne_op, integer_zero_node )
    // And we have an overflow condition
    gg_append_statement( overflow->structs.unstring->into.go_to );
    ELSE
    ENDIF
    }

  if( not_overflow )
    {
    // We have a NOT ON OVERFLOW clause:
    IF( t_overflow, eq_op, integer_zero_node )
    // And there isn't an overflow condition:
    gg_append_statement( not_overflow->structs.unstring->into.go_to );
    ELSE
    ENDIF
    }

  if( overflow )
    {
    gg_append_statement( overflow->structs.unstring->bottom.label );
    free( overflow->structs.unstring );
    }

  if( not_overflow )
    {
    gg_append_statement( not_overflow->structs.unstring->bottom.label );
    free( not_overflow->structs.unstring );
    }
  }

void
parser_call_exception( cbl_label_t *name )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" ")
    SHOW_PARSE_TEXT(name->name)
    SHOW_PARSE_END
    }

  name->structs.call_exception
    = (cbl_call_exception_t *)xmalloc(sizeof(struct cbl_call_exception_t) );

  // Set up the address pairs for this clause
  gg_create_goto_pair(&name->structs.call_exception->over.go_to,
                      &name->structs.call_exception->over.label);
  gg_create_goto_pair(&name->structs.call_exception->into.go_to,
                      &name->structs.call_exception->into.label);
  gg_create_goto_pair(&name->structs.call_exception->bottom.go_to,
                      &name->structs.call_exception->bottom.label);

  // Jump over the [NOT] ON EXCEPTION code that is about to be laid down
  // char ach[128];
  // sprintf(ach, "# parser_call_exception %s: over.goto", name->name);
  // gg_insert_into_assembler(ach);
  SHOW_PARSE
    {
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT("except over.goto")
    SHOW_PARSE_END
    }
  gg_append_statement( name->structs.call_exception->over.go_to );

  // Create the label that allows the following code to be executed at
  // the appropriate time.
  // sprintf(ach, "# parser_call_exception %s: into.label", name->name);
  // gg_insert_into_assembler(ach);
  gg_append_statement( name->structs.call_exception->into.label );
  }

void
parser_call_exception_end( cbl_label_t *name )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(name->name)
    SHOW_PARSE_END
    }
  // char ach[128];
  // sprintf(ach, "# parser_call_exception_end %s: bottom.goto", name->name);
  // gg_insert_into_assembler(ach);
  gg_append_statement( name->structs.call_exception->bottom.go_to );
  }

static
void
create_and_call(size_t narg,
                cbl_ffi_arg_t args[],
                tree function_handle,
                tree returned_value_type,
                cbl_refer_t returned,
                cbl_label_t *not_except
                )
  {
  // We have a good function handle, so we are going to create a call
  tree *arguments = NULL;
  int  *allocated = NULL;

  if(narg)
    {
    arguments = (tree *)xmalloc(2*narg * sizeof(tree));
    allocated = (int * )xmalloc(narg * sizeof(int));
    }

  // Put the arguments onto the "stack" of calling parameters:
  for( size_t i=0; i<narg; i++ )
    {
    cbl_ffi_crv_t crv = args[i].crv;

    if( args[i].refer.field && args[i].refer.field->type == FldLiteralN )
      {
      crv = by_value_e;
      }

    allocated[i] = 0;

    tree location = gg_define_variable(UCHAR_P, "..location.1", vs_stack);
    tree length   = gg_define_variable(SIZE_T,  "..length.1",   vs_stack);

    if( !args[i].refer.field )
      {
      // The PARAMETER is OMITTED
      arguments[i] = null_pointer_node;
      gg_assign(gg_array_value(var_decl_call_parameter_lengths, i),
                size_t_zero_node);
      continue;
      }

    if( refer_is_clean(args[i].refer) )
      {
      if( args[i].refer.field->type == FldLiteralA )
        {
        crv = by_content_e;
        gg_assign(location,
                  gg_cast(UCHAR_P, build_string_literal(args[i].refer.field->data.capacity,
                                       args[i].refer.field->data.initial)));
        gg_assign(length,
                  build_int_cst_type( SIZE_T,
                                      args[i].refer.field->data.capacity));
        }
      else
        {
        gg_assign(location,
                  member(args[i].refer.field->var_decl_node, "data"));
        gg_assign(length,
                  member(args[i].refer.field->var_decl_node, "capacity"));
        }
      }
    else
      {
      gg_assign(location,
                qualified_data_source(args[i].refer)),
      gg_assign(length,
                refer_size_source(args[i].refer));
      }

    switch( crv )
      {
      case by_default_e:
        gcc_unreachable();
        break;

      case by_reference_e:
        {
        arguments[i] = location;

        // Pass the pointer to the data location, so that the called program
        // can both access and change the data.
        break;
        }

      case by_content_e:
        {
        if(    (args[i].refer.field->attr & intermediate_e)
            && is_valuable(args[i].refer.field->type) )
          {
          cbl_unimplemented("CALL USING BY CONTENT <temporary> would require "
                            "REPOSITORY PROTOTYPES.");
          }

        // BY CONTENT means that the called program gets a copy of the data.

        // We'll free this copy after the called program returns.

        switch(args[i].attr)
          {
          case address_of_e:
            {
            // Allocate the memory, and make the copy:
            arguments[i] = gg_define_char_star();
            allocated[i] = 1;
            gg_assign(arguments[i], gg_malloc(length) ) ;
            gg_memcpy(arguments[i],
                      location,
                      length);
            break;
            }

          case length_of_e:
            {
            // The BY CONTENT LENGTH OF gets passed as an 64-bit big-endian
            // value
            arguments[i] = gg_define_size_t();
            allocated[i] = 1;
            gg_assign(arguments[i], gg_malloc(length) ) ;
            gg_call(VOID,
                    "__gg__copy_as_big_endian",
                    gg_get_address_of(arguments[i]),
                    length,
                    NULL_TREE);
            break;
            }

          case none_of_e:
            {
            // Allocate the memory, and make the copy:
            arguments[i] = gg_define_char_star();
            allocated[i] = 1;
            gg_assign(arguments[i], gg_cast(CHAR_P, gg_malloc(length))) ;
            gg_memcpy(arguments[i], location, length);
            break;
            }
          }
        break;
        }

      case by_value_e:
        {
        // For BY VALUE, we take whatever we've been given and do our best to
        // make a 64-bit value out of it, although we move to 128 bits when
        // necessary.
        switch(args[i].attr)
          {
          case address_of_e:
            {
            arguments[i] = gg_define_size_t();
            gg_assign(arguments[i], gg_cast(SIZE_T, location ));
            break;
            }

          case length_of_e:
            {
            arguments[i] = gg_define_size_t();
            gg_assign(arguments[i], gg_cast(SIZE_T, length));
            break;
            }

          case none_of_e:
            {
            assert(args[i].refer.field);
            bool as_int128 = false;
            if( !(args[i].refer.field->attr & intermediate_e) )
              {
              // All temporaries are SIZE_T
              if( args[i].refer.field->type == FldFloat
                  && args[i].refer.field->data.capacity == 16 )
                {
                as_int128 = true;
                }
              else if(   args[i].refer.field->type == FldNumericBin5
                      && args[i].refer.field->data.digits   == 0
                      && args[i].refer.field->data.capacity == 16 )
                {
                as_int128 = true;
                }
              else if( args[i].refer.field->data.digits > 18 )
                {
                as_int128 = true;
                }
              }

            if( as_int128 )
              {
              arguments[i] = gg_define_variable(INT128);
              gg_assign(arguments[i],
                        gg_cast(INT128,
                                gg_call_expr(
                                INT128,
                                "__gg__fetch_call_by_value_value",
                                gg_get_address_of(args[i].refer.field->var_decl_node),
                                refer_offset_source(args[i].refer),
                                refer_size_source(args[i].refer),
                                NULL_TREE)));
              }
            else
              {
              arguments[i] = gg_define_size_t();
              gg_assign(arguments[i],
                        gg_cast(SIZE_T,
                                gg_call_expr(
                                INT128,
                                "__gg__fetch_call_by_value_value",
                                gg_get_address_of(args[i].refer.field->var_decl_node),
                                refer_offset_source(args[i].refer),
                                refer_size_source(args[i].refer),
                                NULL_TREE)));
              }
            break;
            }
          }
        }
      }
    // The elements in this array tell the called routine the length of each
    // variable.  This value is used both to handle ANY LENGTH formal
    // parameters, and to provide information to the called program when being
    // passed expressions BY VALUE and BY CONTENT
    gg_assign(gg_array_value(var_decl_call_parameter_lengths, i),length);
    }

  // Let the called program know how many parameters we are passing
  gg_assign(var_decl_call_parameter_count,
            build_int_cst_type(INT, narg));

  gg_assign(var_decl_call_parameter_signature,
            gg_cast(CHAR_P, function_handle));

  tree call_expr = gg_call_expr_list( returned_value_type,
                                      function_handle,
                                      narg,
                                      arguments );
  tree returned_value;
  if( returned.field )
    {
    returned_value = gg_define_variable(returned_value_type);

    // We are expecting a return value of type CHAR_P, SSIZE_T, SIZE_T,
    // UINT128 or INT128
    push_program_state();
    gg_assign(returned_value, gg_cast(returned_value_type, call_expr));
    pop_program_state();

    // Because the CALL had a RETURNING clause, RETURN-CODE doesn't return a
    // value.  So, we make sure it is zero
    gg_assign(var_decl_return_code, build_int_cst_type(SHORT, 0));

    if( returned_value_type == CHAR_P )
      {
      tree returned_location = gg_define_uchar_star();
      tree returned_length   = gg_define_size_t();
      // we were given a returned::field, so find its location and length:
      gg_assign(returned_location,
                gg_add( member(returned.field->var_decl_node, "data"),
                        refer_offset_dest(returned)));
      gg_assign(returned_length,
                refer_size_dest(returned));

      // The returned value is a string of nbytes, which by specification
      // has to be at least as long as the returned_length of the target:
      IF( returned_value,
          eq_op,
          gg_cast(returned_value_type, null_pointer_node ) )
        {
        // Somebody was discourteous enough to return a NULL pointer
        // We'll jam in spaces:
        gg_memset(  returned_location,
                    char_nodes[(unsigned char)internal_space],
                    returned_length );
        }
      ELSE
        {
        // There is a valid pointer.  Do the assignment.
        move_tree(returned.field,
                  refer_offset_dest(returned),
                  returned_value,
                  integer_one_node);
        }
      ENDIF
      TRACE1
        {
        TRACE1_HEADER
        TRACE1_REFER("returned value: ", returned, "")
        TRACE1_END
        }
      }
    else if(    returned_value_type == SSIZE_T
            ||  returned_value_type == SIZE_T
            ||  returned_value_type == INT128
            ||  returned_value_type == UINT128)
      {
      // We got back a 64-bit or 128-bit integer.  The called and calling
      // programs have to agree on size, but other than that, integer numeric
      // types are converted one to the other.

      gg_call(VOID,
              "__gg__int128_to_qualified_field",
              gg_get_address_of(returned.field->var_decl_node),
              refer_offset_dest(returned),
              refer_size_dest(returned),
              gg_cast(INT128, returned_value),
              gg_cast(INT, member(returned.field->var_decl_node, "rdigits")),
              build_int_cst_type(INT, truncation_e),
              null_pointer_node,
              NULL_TREE );
      TRACE1
        {
        TRACE1_HEADER
        TRACE1_REFER("returned value: ", returned, "")
        TRACE1_END
        }
      }
    else if(    returned_value_type == FLOAT
            ||  returned_value_type == DOUBLE
            ||  returned_value_type == FLOAT128)
      {
      tree returned_location = gg_define_uchar_star();
      tree returned_length   = gg_define_size_t();
      // we were given a returned::field, so find its location and length:
      gg_assign(returned_location,
                qualified_data_source(returned));
      gg_assign(returned_length,
                refer_size_source(returned));

      // We are doing float-to-float, and we require that those be identical
      // one the caller and callee sides.
      gg_memcpy(  returned_location,
                  gg_get_address_of(returned_value),
                  returned_length);

      TRACE1
        {
        TRACE1_HEADER
        TRACE1_REFER("returned value: ", returned, "")
        TRACE1_END
        }
      }
    else
      {
      cbl_internal_error(
            "%s(): What in the name of Nero's fiddle are we doing here?",
            __func__);
      }
    }
  else
    {
    // Because no explicit returning value is expected, we switch to
    // the IBM default behavior, where the returned INT value is assigned
    // to our RETURN-CODE:
    returned_value = gg_define_variable(SHORT);

    // Before doing the call, we save the COBOL program_state:
    push_program_state();
    gg_assign(returned_value, gg_cast(SHORT, call_expr));
    // And after the call, we restore it:
    pop_program_state();

    // We know that the returned value is a 2-byte little-endian INT:
    gg_assign(  var_decl_return_code,
                returned_value);
    TRACE1
      {
      TRACE1_HEADER
      gg_printf("returned value: %d",
                gg_cast(INT, var_decl_return_code),
                NULL_TREE);
      TRACE1_END
      }
    }

  for( size_t i=0; i<narg; i++ )
    {
    if( allocated[i] )
      {
      gg_free(arguments[i]);
      }
    }
  free(arguments);
  free(allocated);

  if( not_except )
    {
    // We have an ON EXCEPT clause:
    gg_append_statement( not_except->structs.call_exception->into.go_to );
    }
  }

void
parser_call(   cbl_refer_t name,
               cbl_refer_t returned,  // This is set by RETURNING clause
               size_t narg,
               cbl_ffi_arg_t args[],
               cbl_label_t *except,
               cbl_label_t *not_except,
               bool /*is_function*/)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD( " calling ", name.field)
    if( except )
      {
      SHOW_PARSE_TEXT(" - except is ")
      SHOW_PARSE_TEXT(except->name)
      }
    if( not_except )
      {
      SHOW_PARSE_TEXT(" - not_except is ")
      SHOW_PARSE_TEXT(not_except->name)
      }
    SHOW_PARSE_TEXT(" (")
    for(size_t i=0; i<narg; i++)
      {
      cbl_field_t *p = args[i].refer.field;
      SHOW_PARSE_FIELD( " ",  p)
      }
    SHOW_PARSE_TEXT(" )")
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_REFER("calling ", name, "");
    for(size_t i=0; i<narg; i++)
      {
      TRACE1_INDENT
      gg_fprintf(trace_handle, 1, "parameter %d: ", build_int_cst_type(INT, i+1));
      switch( args[i].crv )
        {
        case by_default_e: gcc_unreachable();
        case by_reference_e:
          TRACE1_TEXT(" BY REFERENCE ")
          break;
        case by_content_e:
          TRACE1_TEXT(" BY CONTENT ")
          break;
        case by_value_e:
          TRACE1_TEXT(" BY VALUE ")
          break;
        }
      TRACE1_REFER("", args[i].refer, "")
      }
    TRACE1_END
    }

  // If we have an ON EXCEPTION clause, a GOTO was established in
  // parser_call_exception().
  // Here is where we place the label for that GOTO

  if( except )
    {
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT("except over.label:")
      }
    gg_append_statement(except->structs.call_exception->over.label);
    }

  // Likewise, for a NOT ON EXCEPTION
  if( not_except )
    {
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT("not_except over.label:")
      }
    gg_append_statement(not_except->structs.call_exception->over.label);
    }

  // We are getting close to establishing the function_type.  To do that,
  // we want to establish the function's return type.

//  gg_push_context();
  size_t nbytes;
  tree returned_value_type = tree_type_from_field_type(returned.field, nbytes);

  tree function_handle = function_handle_from_name( name,
                                                    returned_value_type);
  if(    (use_static_call() && is_literal(name.field))
      || (name.field && name.field->type == FldPointer) )
    {
    // If these conditions are true, then we know we have a good
    // function_handle, and we don't need to check
    create_and_call(narg,
                    args,
                    function_handle,
                    returned_value_type,
                    returned,
                    not_except
                    );
    }
  else
    {
    // We might not have a good handle, so we have to check:
    IF( function_handle,
        ne_op,
        gg_cast(TREE_TYPE(function_handle), null_pointer_node) )
      {
      create_and_call(narg,
                      args,
                      function_handle,
                      returned_value_type,
                      returned,
                      not_except
                      );
      }
    ELSE
      {
      // We have a bad function pointer, which is the except condition:
      parser_exception_raise(ec_program_not_found_e);
      if( except )
        {
        // We have an ON EXCEPT clause:
        gg_append_statement( except->structs.call_exception->into.go_to );
        // Because there is an ON EXCEPTION clause, suppress DECLARATIVE
        // processing
        gg_assign(var_decl_exception_code, integer_zero_node);
        }
      else
        {
        tree mangled_name = gg_define_variable(CHAR_P);

        gg_call(VOID,
                "__gg__just_mangle_name",
                (name.field->var_decl_node
                                ? gg_get_address_of(name.field->var_decl_node)
                                : null_pointer_node),
                gg_get_address_of(  mangled_name),
                NULL_TREE);

        gg_printf("WARNING: %s:%d \"CALL %s\" not found"
                  " with no \"CALL ON EXCEPTION\" phrase\n",
                  gg_string_literal(current_filename.back().c_str()),
                  build_int_cst_type(INT, CURRENT_LINE_NUMBER),
                  mangled_name,
                  NULL_TREE);
        }
      }
    ENDIF
    }

  // Clean up the label bookkeeping
  if( except )
    {
    gg_append_statement( except->structs.call_exception->bottom.label );
    free( except->structs.call_exception );
    }
  if( not_except )
    {
    gg_append_statement( not_except->structs.call_exception->bottom.label );
    free( not_except->structs.call_exception );
    }
//  gg_pop_context();

  }

// Set global variable to use alternative ENTRY point.
void
parser_entry_activate( size_t iprog, const cbl_label_t *declarative )
  {
  assert(iprog == symbol_elem_of(declarative)->program);
  }

// Define ENTRY point with alternative LINKAGE
void
parser_entry( cbl_field_t */*name*/, size_t /*narg*/, cbl_ffi_arg_t */*args*/ )
  {
  }

void
parser_bitop( struct cbl_field_t *tgt,  // tgt has to be a FldConditional
              struct cbl_field_t *a,    // is modified by SET,CLEAR
              enum bitop_t op,
              size_t bitmask )
  {
  Analyze();
  // This routine is designed to set, clear, and test BITMASK bits in the
  // A operand.  For ON and OFF, it sets tgt, a FldConditional, to TRUE or FALSE

  // This is clumsy:  The ops[] array has to match bitop_t
  static const char *ops[] = { "SET", "CLEAR", "ON", "OFF",
                               "AND", "OR",    "XOR" };
  gcc_assert( op < COUNT_OF(ops) );
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD( " switch: ", a)
    fprintf(stderr, " mask: %lx", bitmask);
    fprintf(stderr, " op: %s", ops[op]);
    SHOW_PARSE_FIELD( " target ", tgt)
    SHOW_PARSE_END
    }

  if(tgt && tgt->type != FldConditional)
    {
    fprintf(stderr,
            "%s(): The target %s has to be a FldConditional, not %s\n",
            __func__,
            tgt->name,
            cbl_field_type_str(tgt->type));
    gcc_unreachable();
    }

  switch(op)
    {
    case bit_set_op:
    case bit_clear_op:
      // For set_on and set_off operations, the tgt is superfluous, so I
      // did this code just in case the parser doesn't give us anything
      // to set
      gg_call(BOOL,
              "__gg__bitop",
              gg_get_address_of(a->var_decl_node),
              build_int_cst_type(INT, op),
              build_int_cst_type(SIZE_T, bitmask),
              NULL_TREE );
      break;

    case bit_on_op:
    case bit_off_op:
      gg_assign(  tgt->var_decl_node,
                  gg_call_expr(   BOOL,
                                  "__gg__bitop",
                                  gg_get_address_of(a->var_decl_node),
                                  build_int_cst_type(INT, op),
                                  build_int_cst_type(SIZE_T, bitmask),
                                  NULL_TREE));
      break;

    case bit_and_op:
    case bit_or_op:
    case bit_xor_op:
      fprintf(stderr,
              "%s(): The %s operation is not valid\n",
              __func__,
              ops[op]);
      gcc_unreachable();
      break;
    }

  TRACE1
    {
    TRACE1_HEADER
    //TRACE1_FIELD_INFO( " target ", tgt)
    TRACE1_FIELD_INFO( " a ", a)
    TRACE1_END
    }
  }

void
parser_bitwise_op(struct cbl_field_t *tgt,
                  struct cbl_field_t *a,
                  enum bitop_t op,
                  size_t bitmask )
  {
  Analyze();
  // This routine is a specialized TGT = A op (size_t) bitmask, where OP is
  // AND, OR, or XOR.  A should be an integer type. tgt should be a valid target
  // for a move where an integer is the sender.

  // SET and CLEAR are straightforward.  ON returns true if any bitmask bit is
  // one in 'A'.  OFF returns true if any bitmask bit in 'A' is zero.

  // This is clumsy:  The ops[] array has to match bitop_t
  static const char *ops[] = { "SET", "CLEAR", "ON", "OFF",
                               "AND", "OR",    "XOR" };
  gcc_assert( op < COUNT_OF(ops) );
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD( " switch: ", a)
    fprintf(stderr, " mask: %lx", bitmask);
    fprintf(stderr, " op: %s", ops[op]);
    SHOW_PARSE_FIELD( " target ", tgt)
    SHOW_PARSE_END
    }

  if( tgt && !is_valuable(tgt->type) && tgt->type != FldLiteralN)
    {
    fprintf(stderr,
            "%s(): The target %s has to be is_valuable, not %s\n",
            __func__,
            tgt->name,
            cbl_field_type_str(tgt->type));
    gcc_unreachable();
    }

  switch(op)
    {
    case bit_set_op:
    case bit_clear_op:
    case bit_on_op:
    case bit_off_op:
      fprintf(stderr,
              "%s(): The %s operation is not valid\n",
              __func__,
              ops[op]);
      gcc_unreachable();
      break;

    case bit_and_op:
    case bit_or_op:
    case bit_xor_op:
      gg_call(VOID,
              "__gg__bitwise_op",
              gg_get_address_of(tgt->var_decl_node),
              gg_get_address_of(a->var_decl_node),
              build_int_cst_type(INT, op),
              build_int_cst_type(SIZE_T, bitmask),
              NULL_TREE );
      break;
    }

  TRACE1
    {
    TRACE1_HEADER
    //TRACE1_FIELD_INFO( " target ", tgt)
    TRACE1_FIELD_INFO( " a ", a)
    TRACE1_END
    }
  }

void
parser_set_pointers( size_t ntgt, cbl_refer_t *tgts, cbl_refer_t source )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD(" source ", source.field);
    char ach[128];
    sprintf(ach,
            " source.addr_of %s",
            source.addr_of ? "TRUE" : "FALSE" );
    SHOW_PARSE_TEXT(ach);
    for( size_t i=0; i<ntgt; i++ )
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_FIELD("target ", tgts[i].field)
      }
    SHOW_PARSE_END
    }
  for( size_t i=0; i<ntgt; i++ )
    {
    if(    !source.addr_of
        && (source.field->type == FldAlphanumeric
            || source.field->type == FldLiteralA))
      {
      // This is something like SET varp TO ENTRY "ref".
      tree function_handle = function_handle_from_name(source,
                                                   COBOL_FUNCTION_RETURN_TYPE);
      gg_memcpy(qualified_data_dest(tgts[i]),
                gg_get_address_of(function_handle),
                sizeof_pointer);
      }
    else
      {
      if( !tgts[i].addr_of )
        {
        // When not ADDRESS OF TARGET, the variable must be a POINTER
        gcc_assert( tgts[i].field->type == FldPointer );
        }
      else
        {
        // When ADDRESS OF TARGET, the target must be linkage or based
        gcc_assert( tgts[i].field->attr & (linkage_e | based_e) );
        }

      gg_call(  VOID,
                "__gg__set_pointer",
                gg_get_address_of(tgts[i].field->var_decl_node),
                refer_offset_dest(tgts[i]),
                build_int_cst_type(INT, tgts[i].addr_of  ? REFER_T_ADDRESS_OF : 0),
                source.field ? gg_get_address_of(source.field->var_decl_node) : null_pointer_node,
                refer_offset_source(source),
                build_int_cst_type(INT, source.addr_of  ? REFER_T_ADDRESS_OF : 0),
                NULL_TREE
                );

      if( tgts[i].addr_of )
        {
        // When SET ADDRESS OF TARGET TO ..., the library call sets
        // tgts[i].field->data.  We need to propogate the data+offset
        // through the level01 variable's children:
        propogate_linkage_offsets(tgts[i].field,
                                  member(tgts[i].field->var_decl_node, "data"));
        }
      }
    }
  }
typedef struct hier_node
  {
  size_t our_index;     // In the symbol table
  bool   common;
  struct hier_node *parent_node;
  char *name;
  std::vector<struct hier_node *>child_nodes;

  hier_node() :
    our_index(0),
    common(false),
    parent_node(NULL)
    {}
  } hier_node;

static hier_node *
find_hier_node( const std::unordered_map<size_t, hier_node *> &node_map,
                size_t program_index)
  {
  std::unordered_map<size_t, hier_node *>::const_iterator it =
        node_map.find(program_index);
  if( it == node_map.end() )
    {
    return NULL;
    }
  return it->second;
  }

static bool
sort_by_hier_name(const hier_node *a, const hier_node *b)
  {
  return strcmp(a->name, b->name) < 0;
  }

static void
find_uncles(const hier_node *node, std::vector<const hier_node *> &uncles)
  {
  const hier_node *parent = node->parent_node;
  if( parent )
    {
    for(size_t i=0; i<parent->child_nodes.size(); i++)
      {
      if( parent->child_nodes[i] != node )
        {
        if( parent->child_nodes[i]->common )
          {
          uncles.push_back(parent->child_nodes[i]);
          }
        }
      }
    find_uncles(parent, uncles);
    }
  }

void
parser_program_hierarchy( const struct cbl_prog_hier_t& hier )
  {
  Analyze();
  /*  The complication in this routine is that it gets called near the end
      of every program-id.  And it keeps growing.  The reason is because the
      parser doesn't know when it is working on the last program of a list of
      nested programs.  So, we just do what we need to do, and we keep track
      of what we've already built so that we don't build it more than once.
      */
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( gg_trans_unit.function_stack.size() != 1 )
      {
      SHOW_PARSE_TEXT("Ending a nested function")
      }
    else
      {
      for( size_t i=0; i<hier.nlabel; i++ )
        {
        if( i )
          {
          SHOW_PARSE_INDENT
          }
        else
          {
          SHOW_PARSE_TEXT(" ");
          }
        char ach[128];
        sprintf(ach,
                "%ld %s%s parent:%ld",
                hier.labels[i].ordinal,
                hier.labels[i].label.name,
                hier.labels[i].label.common ? " COMMON" : "",
                hier.labels[i].label.parent);
        SHOW_PARSE_TEXT(ach);
        }
      }
    SHOW_PARSE_END
    }

  // This needs to be an island that doesn't execute in-line.  This is necessary
  // when there isn't a GOBACK or GOTO or STOP RUN at the point where a
  // [possibly implicit] PROGRAM END is encountered
  tree skipper_goto;
  tree skipper_label;
  gg_create_goto_pair(&skipper_goto,
                      &skipper_label);
  gg_append_statement(skipper_goto);

  // The stack.size() test shouldn't be necessary, because the parser should
  // be calling us only at the PROGRAM END point of an outermost function.

  gcc_assert(gg_trans_unit.function_stack.size() == 1);

  gg_append_statement(label_list_out_label);

  std::unordered_map<size_t, std::vector<const hier_node *>> map_of_lists;
  std::unordered_map<size_t, hier_node *> node_map;
  std::vector<hier_node *> nodes;

  // We need to avoid duplicating names, because a direct child's name takes
  // precedence over a COMMON name above us in the hierarchy:

  std::unordered_map<size_t, std::unordered_set<std::string>>map_of_sets;

  // We need to build a tree out of the hierarchical structure:
  // Create, essentially, a root node:
  hier_node *zero_node = new hier_node;
  nodes.push_back(zero_node);
  node_map[0] = nodes.back();

  // Pass 1: Create a node for every program:
  for( size_t i=0; i<hier.nlabel; i++ )
    {
    hier_node *existing_node = find_hier_node(node_map, hier.labels[i].ordinal);
    gcc_assert( existing_node == NULL );

    hier_node *new_node = new hier_node;
    new_node->our_index    = hier.labels[i].ordinal;
    new_node->common       = hier.labels[i].label.common;
    new_node->name         = cobol_name_mangler(hier.labels[i].label.name);
    nodes.push_back(new_node);
    node_map[hier.labels[i].ordinal] = nodes.back();
    }

  // Pass 2: populate each node with their parent and children:
  for( size_t i=0; i<hier.nlabel; i++ )
    {
    hier_node *child_node = find_hier_node(node_map, hier.labels[i].ordinal);
    gcc_assert(child_node);

    hier_node *parent_node = find_hier_node(node_map,
                                            hier.labels[i].label.parent);
    gcc_assert(parent_node);

    child_node->parent_node = parent_node;
    parent_node->child_nodes.push_back(child_node);
    }

  // We now build the lists of routines that can be called from every routine

  // We are going to create one vector of hier_nodes for each routine:

  for(size_t i=0; i<nodes.size(); i++)
    {
    // First, direct children always take precedence
    size_t caller = nodes[i]->our_index;
    const hier_node *caller_node = nodes[i];
    for(size_t j=0; j<caller_node->child_nodes.size(); j++)
      {
      map_of_lists[caller].push_back(caller_node->child_nodes[j]);
      map_of_sets[caller].insert(caller_node->child_nodes[j]->name);
      }

    // Sibling routines marked COMMON, and siblings of ancestors marked COMMON
    // are also accessible by us.  Go find them.
    std::vector<const hier_node *>uncles;
    find_uncles(nodes[i], uncles);
    for( size_t i=0; i<uncles.size(); i++ )
      {
      const hier_node *uncle = uncles[i];
      if( map_of_sets[caller].find(uncle->name) == map_of_sets[caller].end() )
        {
        // We have a COMMON uncle or sibling we haven't seen before.
        map_of_lists[caller].push_back(uncle);
        }
      }
    }

  // Having created lists of callables for each caller, we want to sort each
  // of those lists to make it easier to bsearch things in them later:
  for(  std::unordered_map<size_t, std::vector<const hier_node *>>::iterator mol = map_of_lists.begin();
        mol != map_of_lists.end();
        mol++ )
    {
    std::sort(mol->second.begin(), mol->second.end(), sort_by_hier_name);
    }

  // Having built the lists of lists, start pulling them apart

  tree function_type =
    build_varargs_function_type_array( SIZE_T,
                                       0,     // No parameters yet
                                       NULL); // And, hence, no types
  tree pointer_type = build_pointer_type(function_type);

  static std::unordered_set<size_t>callers;

  for(  std::unordered_map<size_t, std::vector<const hier_node *>>::const_iterator mol = map_of_lists.begin();
        mol != map_of_lists.end();
        mol++ )
    {
    size_t caller = mol->first;
    if( caller != 0 )
      {
      if( callers.find(caller) == callers.end() )
        {
        // We haven't seen this caller before
        callers.insert(caller);

        char ach[2*sizeof(cbl_name_t)];
        tree names_table_type = build_array_type_nelts(CHAR_P, mol->second.size()+1);
        sprintf(ach, "..our_accessible_functions_%ld", caller);
        tree the_names_table = gg_define_variable(names_table_type, ach, vs_file_static);

        // Here is where we build a table out of constructors:
        tree constructed_array_type   = build_array_type_nelts(pointer_type, mol->second.size());
        sprintf(ach, "..our_constructed_table_%ld", caller);
        tree the_constructed_table = gg_define_variable(constructed_array_type, ach, vs_file_static);

        tree constr_names = make_node(CONSTRUCTOR);
        TREE_TYPE(constr_names) = names_table_type;
        TREE_STATIC(constr_names)    = 1;
        TREE_CONSTANT(constr_names)  = 1;

        tree constr = make_node(CONSTRUCTOR);
        TREE_TYPE(constr) = constructed_array_type;
        TREE_STATIC(constr)    = 1;
        TREE_CONSTANT(constr)  = 1;

        int i=0;
        for(  std::vector<const hier_node *>::const_iterator callee = mol->second.begin();
              callee != mol->second.end();
              callee++ )
          {
          sprintf(ach, "%s.%ld", (*callee)->name, (*callee)->parent_node->our_index);

          CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr_names),
                                  build_int_cst_type(SIZE_T, i),
                                  build_string_literal(ach));

          // Build the constructor element for that function:
          tree function_decl = build_fn_decl (ach, function_type);
          tree addr_expr = build1(ADDR_EXPR, pointer_type, function_decl);

          CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr),
                                  build_int_cst_type(SIZE_T, i),
                                  addr_expr);

          i++;
          }
        // Terminate the names table with NULL
        CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr_names),
                                build_int_cst_type(SIZE_T, i),
                                null_pointer_node);

        DECL_INITIAL(the_names_table) = constr_names;
        DECL_INITIAL(the_constructed_table) = constr;

        // And put a pointer to that table into the file-static variable set aside
        // for it:
        sprintf(ach, "..accessible_program_list_%ld", caller);
        tree accessible_list_var_decl = gg_trans_unit_var_decl(ach);
        gg_assign( accessible_list_var_decl, gg_get_address_of(the_names_table) );

        sprintf(ach, "..accessible_program_pointers_%ld", caller);
        tree accessible_programs_decl = gg_trans_unit_var_decl(ach);
        gg_assign( accessible_programs_decl, gg_get_address_of(the_constructed_table) );
        }
      }
    }
  gg_append_statement(label_list_back_goto);
  gg_append_statement(skipper_label);
  }

void
parser_set_handled(ec_type_t ec_handled)
  {
  if( mode_syntax_only() ) return;
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    char ach[64];
    sprintf(ach, "ec_type_t: 0x%lx", size_t(ec_handled));
    SHOW_PARSE_TEXT(ach);
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }

  if( gg_trans_unit.function_stack.size() )
    {
    if( ec_handled )
      {
      // We assume that exception_handled is zero, always.  We only make it
      // non-zero when something needs to be done.  __gg__match_exception is
      // in charge of setting it back to zero.
      gg_assign(var_decl_exception_handled,
                build_int_cst_type(INT, (int)ec_handled));
      }
    }
  else
    {
    yywarn("parser_set_handled() called between programs");
    }
  }

void
parser_set_file_number(int file_number)
  {
  if( mode_syntax_only() ) return;
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    char ach[32];
    sprintf(ach, "file number: %d", file_number);
    SHOW_PARSE_TEXT(ach);
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }

  if( gg_trans_unit.function_stack.size() )
    {
    gg_assign(var_decl_exception_file_number,
              build_int_cst_type(INT, file_number));
    }
  else
    {
    yywarn("parser_set_file_number() called between programs");
    }
  }

void
parser_set_numeric(struct cbl_field_t *tgt, ssize_t value)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" set ")
    SHOW_PARSE_TEXT(tgt->name)
    SHOW_PARSE_TEXT(" to ")
    char ach[32];
    sprintf(ach, "%ld", value);
    SHOW_PARSE_TEXT(ach);
    SHOW_PARSE_END
    }

  gg_call(VOID,
          "__gg__int128_to_field",
          gg_get_address_of(tgt->var_decl_node),
          build_int_cst_type(INT128, value),
          integer_zero_node,
          build_int_cst_type(INT, truncation_e),
          null_pointer_node,
          NULL_TREE );
  }

static void
stash_exceptions( const cbl_enabled_exceptions_array_t *enabled )
  {
  // We need to create a static array of bytes
  size_t narg = enabled->nbytes();
  unsigned char *p = (unsigned char *)(enabled->ecs);

  static size_t prior_narg = 0;
  static size_t max_narg   = 128;
  static unsigned char *prior_p = (unsigned char *)xmalloc(max_narg);

  bool we_got_new_data = false;
  if( prior_narg != narg )
    {
    we_got_new_data = true;
    }
  else
    {
    // The narg counts are the same.
    for(size_t i=0; i<narg; i++)
      {
      if( p[i] != prior_p[i] )
        {
        we_got_new_data = true;
        break;
        }
      }
    }

  if( !we_got_new_data )
    {
    return;
    }

  if( narg > max_narg )
    {
    max_narg = narg;
    prior_p = (unsigned char *)xrealloc(prior_p, max_narg);
    }

  memcpy(prior_p, p, narg);

  static int count = 1;

  tree array_of_chars_type;
  tree array_of_chars;

  if( narg )
    {
    char ach[32];
    sprintf(ach, "_ec_array_%d", count++);
    array_of_chars_type = build_array_type_nelts(UCHAR, narg);

    // We have the array.  Now we need to build the constructor for it
    tree constr = make_node(CONSTRUCTOR);
    TREE_TYPE(constr) = array_of_chars_type;
    TREE_STATIC(constr)    = 1;
    TREE_CONSTANT(constr)  = 1;

    for(size_t i=0; i<narg; i++)
      {
      CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr),
                              build_int_cst_type(SIZE_T, i),
                              build_int_cst_type(UCHAR, p[i]));
      }
    array_of_chars = gg_define_variable(array_of_chars_type, ach, vs_static);
    DECL_INITIAL(array_of_chars) = constr;

    gg_call(VOID,
            "__gg__stash_exceptions",
            build_int_cst_type(SIZE_T, enabled->nec),
            narg ? gg_get_address_of(array_of_chars) : null_pointer_node,
            NULL_TREE);
    }
  }

static void
store_location_stuff(const cbl_name_t statement_name)
  {
  if( exception_location_active && !current_declarative_section_name() )
    {
    // We need to establish some stuff for EXCEPTION- function processing
    gg_assign(var_decl_exception_source_file,
              gg_string_literal(current_filename.back().c_str()));

    gg_assign(var_decl_exception_program_id,
              gg_string_literal(current_function->our_unmangled_name));

    if( strstr(current_function->current_section->label->name, "_implicit")
        != current_function->current_section->label->name )
      {
      gg_assign(var_decl_exception_section,
           gg_string_literal(current_function->current_section->label->name));
      }
    else
      {
      gg_assign(var_decl_exception_section,
                gg_cast(build_pointer_type(CHAR_P),null_pointer_node));
      }

    if( strstr(current_function->current_paragraph->label->name, "_implicit")
        != current_function->current_paragraph->label->name )
      {
      gg_assign(var_decl_exception_paragraph,
           gg_string_literal(current_function->current_paragraph->label->name));
      }
    else
      {
      gg_assign(var_decl_exception_paragraph,
                gg_cast(build_pointer_type(CHAR_P), null_pointer_node));
      }

    gg_assign(var_decl_exception_source_file,
              gg_string_literal(current_filename.back().c_str()));
    gg_assign(var_decl_exception_line_number, build_int_cst_type(INT,
                                                          CURRENT_LINE_NUMBER));
    gg_assign(var_decl_exception_statement, gg_string_literal(statement_name));
    }
  }

void
parser_exception_prepare( const cbl_name_t statement_name,
                          const cbl_enabled_exceptions_array_t *enabled )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(enabled->nec? " stashing " : " skipping ")
    SHOW_PARSE_TEXT(statement_name)
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }

  if( enabled->nec )
    {
    if( gg_trans_unit.function_stack.size() )
      {
      stash_exceptions(enabled);
      store_location_stuff(statement_name);
      }
    else
      {
      yywarn("parser_exception_prepare() called between programs");
      }
    }
  }

void
parser_exception_clear()
  {
  if( mode_syntax_only() ) return;

  Analyze();
  gg_assign(var_decl_exception_code, integer_zero_node);
  }

void
parser_exception_raise(ec_type_t ec)
  {
  Analyze();
  if( ec == ec_none_e )
    {
    gg_call(VOID,
            "__gg__set_exception_code",
            integer_zero_node,
            integer_one_node,
            NULL_TREE);
    }
  else
    {
    set_exception_code_func(ec, __LINE__, 1);
    }
  }

void
parser_match_exception(cbl_field_t *index,
                       cbl_field_t *blob )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD(" index   ", index)
    SHOW_PARSE_INDENT
    if( blob )
      {
      SHOW_PARSE_FIELD("blob    ", blob)
      }
    else
      {
      SHOW_PARSE_TEXT("blob    is NULL")
      }
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_FIELD("index   ", index, "")
    TRACE1_INDENT
    TRACE1_TEXT("blob    ")
    if( blob )
      {
      TRACE1_TEXT(blob->name)
      }
    else
      {
      TRACE1_TEXT("is NULL")
      }
    TRACE1_END
    }

  gg_call(VOID,
          "__gg__match_exception",
          gg_get_address_of(index->var_decl_node),
          blob ? blob->var_decl_node : null_pointer_node,
          NULL_TREE);

  TRACE1
    {
    static tree index_val = gg_define_variable(INT, "..pme_index", vs_file_static);
    get_binary_value(index_val, NULL, index, size_t_zero_node);
    TRACE1_INDENT
    gg_printf("returned value is 0x%x (%d)", index_val, index_val, NULL_TREE);
    TRACE1_END
    }
  }

void
parser_check_fatal_exception()
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" Check for fatal EC...")
    SHOW_PARSE_END
    }
  gg_call(VOID,
          "__gg__check_fatal_exception",
          NULL_TREE);
  }

void
parser_clear_exception()
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" Clear raised EC...")
    SHOW_PARSE_END
    }
  gg_call(VOID, "__gg__clear_exception", NULL_TREE);
  }

void
parser_exception_file( cbl_field_t *tgt, cbl_file_t *file)
  {
  Analyze();
  gg_call(VOID,
          "__gg__func_exception_file",
          gg_get_address_of(tgt->var_decl_node),
          file ? gg_get_address_of(file->var_decl_node) : null_pointer_node,
          NULL_TREE);
  }

void
parser_file_stash( struct cbl_file_t *file )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if(file)
      {
      SHOW_PARSE_TEXT(" ");
      SHOW_PARSE_TEXT(file->name);
      }
    else
      {
      SHOW_PARSE_TEXT(" *file is NULL ")
      }
    SHOW_PARSE_END
    }

  if( file )
    {
    TRACE1
      {
      TRACE1_HEADER
      TRACE1_TEXT("parser_file_stash of ")
      TRACE1_TEXT(file->name);
      TRACE1_END
      }

    gg_call(VOID,
            "__gg__file_stash",
            gg_get_address_of(file->var_decl_node),
            NULL_TREE);
    }
  else
    {
    TRACE1
      {
      TRACE1_HEADER
      TRACE1_TEXT("parser_file_stash of NULL ")
      TRACE1_END
      }

    gg_call(VOID,
            "__gg__file_stash",
            null_pointer_node,
            NULL_TREE);
    }
  }

static void
hijack_for_development(const char *funcname)
  {
  /*

  To make sure that things like global symbols and whatnot get initialized, you
  should probably create a source file that looks like this:

        identification division.
        program-id. prog.
        procedure division.
        call "dubner".
        end program prog.
        identification division.
        program-id. dubner.
        procedure division.
        goback.
        end program dubner.

  The first program will cause all of the parser_enter_program() and
  parser_division(procedure_div_e) stuff to be initialized.  The second program,
  named "dubner", will be hijacked and bring you here.  */

  // Assume that funcname is lowercase with no hyphens
  enter_program_common(funcname, funcname);
  parser_display_literal("You have been hijacked by a program named \"dubner\"");
  gg_insert_into_assembler("%s HIJACKED DUBNER CODE START", ASM_COMMENT_START);

  for(int i=0; i<10; i++)
    {
    char ach[64];
    sprintf(ach, "Hello, world - %d", i+1);

    gg_call(VOID,
            "puts",
            build_string_literal(strlen(ach)+1, ach),
            NULL_TREE);
    }

  gg_insert_into_assembler("%s HIJACKED DUBNER CODE END", ASM_COMMENT_START);
  gg_return(0);
  }

static void
conditional_abs(tree source, cbl_field_t *field)
  {
  Analyze();
  if( !(field->attr & signable_e) )
    {
    gg_assign(source, gg_abs(source));
    }
  }

static bool
mh_identical(cbl_refer_t &destref,
             cbl_refer_t &sourceref,
             TREEPLET    &tsource)
  {
  // Check to see if the two variables are identical types, thus allowing
  // for a simple byte-for-byte copy of the data areas:
  bool moved = false;
  if(     destref.field->type          == sourceref.field->type
      &&  destref.field->data.capacity == sourceref.field->data.capacity
      &&  destref.field->data.digits   == sourceref.field->data.digits
      &&  destref.field->data.rdigits  == sourceref.field->data.rdigits
      &&       (destref.field->attr   & (signable_e|separate_e|leading_e))
            == (sourceref.field->attr & (signable_e|separate_e|leading_e))
      &&  !destref.field->occurs.depending_on
      &&  !sourceref.field->occurs.depending_on
      &&  !destref.refmod.from
      &&  !sourceref.refmod.len
      &&  !(destref.field->attr   & intermediate_e) // variables with variable
      &&  !(sourceref.field->attr & intermediate_e) // capacities have to be
      &&  !(destref.field->attr   & any_length_e)   // handled elsewhere
      &&  !(sourceref.field->attr & any_length_e)
      )
    {
    // The source and destination are identical in type
    if( (sourceref.field->attr & intermediate_e) || !symbol_find_odo(sourceref.field) )
      {
      Analyze();
      // Source doesn't have a depending_on clause
      SHOW_PARSE1
        {
        SHOW_PARSE_INDENT
        SHOW_PARSE_TEXT("mh_identical()");
        }
      gg_memcpy(gg_add(member(destref.field->var_decl_node,   "data"),
                       refer_offset_dest(destref)),
                gg_add(member(sourceref.field->var_decl_node, "data"),
                       tsource.offset),
                build_int_cst_type(SIZE_T, sourceref.field->data.capacity));
      moved = true;
      }
    }
  return moved;
  }

static bool
mh_source_is_literalN(cbl_refer_t &destref,
                      cbl_refer_t &sourceref,
                      bool         check_for_error,
                      cbl_round_t  rounded,
                      tree         size_error)
  {
  bool moved = false;
  if( sourceref.field->type == FldLiteralN )
    {
    Analyze();
    switch( destref.field->type )
      {
      case FldGroup:
      case FldAlphanumeric:
        {
        SHOW_PARSE1
          {
          SHOW_PARSE_INDENT
          SHOW_PARSE_TEXT("mh_source_is_literalN: __gg__psz_to_alpha_move")
          }

        static  char *buffer = NULL;
        static size_t buffer_size = 0;
        raw_to_internal(&buffer,
                        &buffer_size,
                        sourceref.field->data.initial,
                        strlen(sourceref.field->data.initial));
        gg_call(VOID,
                "__gg__psz_to_alpha_move",
                gg_get_address_of(destref.field->var_decl_node),
                refer_offset_dest(destref),
                refer_size_dest(destref),
                gg_string_literal(buffer),
                build_int_cst_type(SIZE_T, strlen(sourceref.field->data.initial)),
                NULL_TREE);
        moved = true;
        break;
        }

      case FldPointer:
      case FldIndex:
        {
        // We know this is a move to an eight-byte value:
        SHOW_PARSE1
          {
          SHOW_PARSE_INDENT
          SHOW_PARSE_TEXT("mh_source_is_literalN: pointer/index")
          }

        if( sourceref.field->data.capacity < 8 )
          {
          // There are too few bytes in sourceref
          if( sourceref.field->attr & signable_e )
            {
            static tree highbyte = gg_define_variable(UCHAR, "..mh_litN_highbyte", vs_file_static);
            // Pick up the source byte that has the sign bit.
            gg_assign(highbyte,
                      gg_get_indirect_reference(gg_add(member(sourceref.field->var_decl_node,
                                                              "data"),
                                                build_int_cst_type(SIZE_T,
                                                                   sourceref.field->data.capacity-1)),
                      integer_zero_node));
            IF( gg_bitwise_and(highbyte, build_int_cst_type(UCHAR, 0x80)),
                eq_op,
                build_int_cst_type(UCHAR, 0x80) )
              {
              // We are dealing with a negative number
              gg_memset(gg_add(member(destref.field->var_decl_node, "data"),
                               refer_offset_dest(destref)),
                                build_int_cst_type(UCHAR, 0xFF),
                                build_int_cst_type(SIZE_T, 8));
              }
            ELSE
              gg_memset(gg_add(member(destref.field->var_decl_node, "data"),
                               refer_offset_dest(destref)),
                                build_int_cst_type(UCHAR, 0x00),
                                build_int_cst_type(SIZE_T, 8));
              ENDIF
            }
          else
            {
            // The too-short source is positive.
              gg_memset(gg_add(member(destref.field->var_decl_node, "data"),
                               refer_offset_dest(destref)),
                              build_int_cst_type(UCHAR, 0x00),
                              build_int_cst_type(SIZE_T, 8));
            }
          }

        tree literalN_value = get_literalN_value(sourceref.field);
        scale_by_power_of_ten_N(literalN_value, -sourceref.field->data.rdigits);
        gg_memcpy(gg_add(member(destref.field->var_decl_node, "data"),
                               refer_offset_dest(destref)),
                  gg_get_address_of(literalN_value),
                  build_int_cst_type(SIZE_T, sourceref.field->data.capacity));
        moved = true;

        break;
        }

      case FldNumericBin5:
        {
        // We are moving from a FldLiteralN (which we know has no subscripts or
        // refmods), to a NumericBin5, which might.

        SHOW_PARSE1
          {
          SHOW_PARSE_INDENT
          SHOW_PARSE_TEXT("mh_source_is_literalN: FldNumericBin5")
          }

        // For now, we are ignoring intermediates:
        assert( !(destref.field->attr & intermediate_e) );

        int bytes_needed = std::max(destref.field->data.capacity,
                                    sourceref.field->data.capacity);
        tree calc_type = tree_type_from_size(bytes_needed,
                                            sourceref.field->attr & signable_e);
        tree dest_type = tree_type_from_size( destref.field->data.capacity,
                                              destref.field->attr & signable_e);

        // Pick up the source data.
        tree source = gg_define_variable(calc_type);
        gg_assign(source, gg_cast(calc_type, sourceref.field->data_decl_node));

        // Take the absolute value, if the destination is not signable
        conditional_abs(source, destref.field);

        // See if it needs to be scaled:
        scale_by_power_of_ten_N(
                     source,
                     destref.field->data.rdigits-sourceref.field->data.rdigits);

        if( check_for_error && size_error )
          {
          Analyzer.Message("Check to see if result fits");
          if( destref.field->data.digits )
            {
            FIXED_WIDE_INT(128) power_of_ten = get_power_of_ten(destref.field->data.digits);
            IF( gg_abs(source), ge_op, wide_int_to_tree(calc_type,
                                                        power_of_ten) )
              {
              gg_assign(size_error, gg_bitwise_or(size_error, integer_one_node));
              }
            ELSE
              ENDIF
            }
          }

        Analyzer.Message("Move to destination location");
        tree dest_location = gg_indirect(
                    gg_cast(build_pointer_type(dest_type),
                            gg_add(member(destref.field->var_decl_node, "data"),
                                   refer_offset_dest(destref))));
        gg_assign(dest_location, gg_cast(dest_type, source));
        moved = true;
        break;
        }

      case FldNumericDisplay:
      case FldNumericBinary:
      case FldNumericEdited:
      case FldPacked:
        {
        static tree berror = gg_define_variable(INT, "..mh_litN_berror", vs_file_static);
        gg_assign(berror, integer_zero_node);
        SHOW_PARSE1
          {
          SHOW_PARSE_INDENT
          SHOW_PARSE_TEXT("calling get_literalN_value ")
          }
        tree literalN_value = get_literalN_value(sourceref.field);

        SHOW_PARSE1
          {
          SHOW_PARSE_INDENT
          SHOW_PARSE_TEXT("calling __gg__int128_to_qualified_field ")
          }

        gg_call(INT,
                "__gg__int128_to_qualified_field",
                gg_get_address_of(destref.field->var_decl_node),
                refer_offset_dest(destref),
                refer_size_dest(destref),
                gg_cast(INT128, literalN_value),
                build_int_cst_type(INT, sourceref.field->data.rdigits),
                build_int_cst_type(INT, rounded),
                gg_get_address_of(berror),
                NULL_TREE);

        if( size_error )
          {
          IF( berror, ne_op, integer_zero_node  )
            {
            gg_assign(size_error, gg_bitwise_or(size_error, integer_one_node));
            }
          ELSE
            ENDIF
          }
        moved = true;
        break;
        }

      case FldAlphaEdited:
        {
        SHOW_PARSE1
          {
          SHOW_PARSE_INDENT
          SHOW_PARSE_TEXT(" FldAlphaEdited")
          }
        gg_call(VOID,
                "__gg__string_to_alpha_edited_ascii",
                gg_add( member(destref.field->var_decl_node, "data"),
                        refer_offset_dest(destref) ),
                gg_string_literal(sourceref.field->data.initial),
                build_int_cst_type(INT, strlen(sourceref.field->data.initial)),
                gg_string_literal(destref.field->data.picture),
                NULL_TREE);
        moved = true;
        break;
        }

      case FldFloat:
        {
        tree tdest = gg_add(member(destref.field->var_decl_node, "data"),
                            refer_offset_dest(destref) );
        switch( destref.field->data.capacity )
          {
          // For some reason, using FLOAT128 in the build_pointer_type causes
          // a SEGFAULT.  So, we'll use other types with equivalent sizes. I
          // am speculating that the use of floating-point types causes the -O0
          // compilation to move things using the mmx registers.  So, I am using
          // intxx types in the hope that they are simpler.
          case 4:
            {
            // The following generated code is the exact equivalent
            // of the C code:
            //   *(float *)dest = (float)data.value
            gg_assign(gg_indirect(gg_cast(build_pointer_type(FLOAT), tdest)),
                      fold_convert (FLOAT, sourceref.field->data.value_of()));
            break;
            }
          case 8:
            {
            gg_assign(gg_indirect(gg_cast(build_pointer_type(DOUBLE), tdest)),
                      fold_convert (DOUBLE, sourceref.field->data.value_of()));
            break;
            }
          case 16:
            {
            gg_assign(gg_indirect(gg_cast(build_pointer_type(FLOAT128), tdest)),
                      sourceref.field->data.value_of());
            break;
            }
          }
        moved=true;
        break;
        }

      default:
        cbl_internal_error(
              "In parser_move(%s to %s), the move of FldLiteralN to %s "
              "hasn't been implemented",
              sourceref.field->name,
              destref.field->name,
              cbl_field_type_str(destref.field->type));
        break;
      }
    }
  return moved;
  }

static
tree float_type_of(int n)
  {
  switch(n)
    {
    case 4:
      return FLOAT;
    case 8:
      return DOUBLE;
    case 16:
      return FLOAT128;
    default:
      gcc_unreachable();
    }
  return NULL_TREE;
  }

static tree
float_type_of(cbl_field_t *field)
  {
  gcc_assert(field->type == FldFloat);
  return float_type_of(field->data.capacity);
  }

static tree
float_type_of(cbl_refer_t *refer)
  {
  return float_type_of(refer->field);
  }

static bool
mh_dest_is_float( cbl_refer_t &destref,
                  cbl_refer_t &sourceref,
                  TREEPLET    &tsource,
                  cbl_round_t    rounded,
                  tree         size_error) // int
  {
  bool moved = false;
  if( destref.field->type == FldFloat )
    {
    Analyze();
    switch( sourceref.field->type )
      {
      case FldPointer:
      case FldIndex:
      case FldNumericBin5:
      case FldNumericDisplay:
      case FldNumericBinary:
      case FldNumericEdited:
      case FldPacked:
        {
        switch( destref.field->data.capacity )
          {
          case 4:
            gg_call(VOID,
                    "__gg__float32_from_int128",
                    gg_get_address_of(destref.field->var_decl_node),
                    refer_offset_dest(destref),
                    tsource.pfield,
                    tsource.offset,
                    build_int_cst_type(INT, rounded),
                    size_error ? gg_get_address_of(size_error) : null_pointer_node,
                    NULL_TREE);
            break;
          case 8:
            gg_call(VOID,
                    "__gg__float64_from_int128",
                    gg_get_address_of(destref.field->var_decl_node),
                    refer_offset_dest(destref),
                    tsource.pfield,
                    tsource.offset,
                    build_int_cst_type(INT, rounded),
                    size_error ? gg_get_address_of(size_error) : null_pointer_node,
                    NULL_TREE);
            break;
          case 16:
            gg_call(VOID,
                    "__gg__float128_from_int128",
                    gg_get_address_of(destref.field->var_decl_node),
                    refer_offset_dest(destref),
                    tsource.pfield,
                    tsource.offset,
                    build_int_cst_type(INT, rounded),
                    size_error ? gg_get_address_of(size_error) : null_pointer_node,
                    NULL_TREE);
            break;
          }
        moved = true;
        break;
        }

      case FldFloat:
        {
        // We are testing for size.  First, we need to check to see if the
        // source is INFINITY.  If so, that's an automatic size error

        IF( gg_call_expr( INT,
                          "__gg__is_float_infinite",
                          tsource.pfield,
                          tsource.offset,
                          NULL_TREE),
            ne_op,
            integer_zero_node )
          {
          if( size_error )
            {
            gg_assign(size_error, integer_one_node );
            }
          }
        ELSE
          {
          // The source isn't infinite.
          // If the destination is bigger than the source, then we can
          // do an untested move:

          if( destref.field->data.capacity >= sourceref.field->data.capacity )
            {
            tree dtype = float_type_of(&destref);
            tree stype = float_type_of(&sourceref);

            tree tdest = gg_add(member(destref.field->var_decl_node, "data"),
                               refer_offset_dest(destref));
            tree source = gg_add(member(sourceref.field->var_decl_node, "data"),
                                refer_offset_source(sourceref));
            gg_assign(gg_indirect(gg_cast(build_pointer_type(dtype), tdest)),
                      gg_cast(dtype,
                              gg_indirect(gg_cast(build_pointer_type(stype),
                                          source))));
            }
          else
            {
            // There are only three possible moves left:
            if(destref.field->data.capacity == 8 )
              {
              if( size_error )
                {
                gg_assign(size_error,
                          gg_call_expr( INT,
                                "__gg__float64_from_128",
                                gg_get_address_of(destref.field->var_decl_node),
                                refer_offset_dest(destref),
                                tsource.pfield,
                                tsource.offset,
                                NULL_TREE));
                }
              else
                {
                          gg_call( INT,
                                "__gg__float64_from_128",
                                gg_get_address_of(destref.field->var_decl_node),
                                refer_offset_dest(destref),
                                tsource.pfield,
                                tsource.offset,
                                NULL_TREE);
                }
              }
            else
              {
              // The destination has to be float32
              if( sourceref.field->data.capacity == 8 )
                {
                if( size_error )
                  {
                  gg_assign(size_error,
                            gg_call_expr( INT,
                                "__gg__float32_from_64",
                                gg_get_address_of(destref.field->var_decl_node),
                                refer_offset_dest(destref),
                                tsource.pfield,
                                tsource.offset,
                                NULL_TREE));
                  }
                else
                  {
                            gg_call( INT,
                                "__gg__float32_from_64",
                                gg_get_address_of(destref.field->var_decl_node),
                                refer_offset_dest(destref),
                                tsource.pfield,
                                tsource.offset,
                                NULL_TREE);
                  }

                }
              else
                {
                if( size_error )
                  {
                  gg_assign(size_error,
                            gg_call_expr( INT,
                                "__gg__float32_from_128",
                                gg_get_address_of(destref.field->var_decl_node),
                                refer_offset_dest(destref),
                                tsource.pfield,
                                tsource.offset,
                                NULL_TREE));
                  }
                else
                  {
                            gg_call( INT,
                                "__gg__float32_from_128",
                                gg_get_address_of(destref.field->var_decl_node),
                                refer_offset_dest(destref),
                                tsource.pfield,
                                tsource.offset,
                                NULL_TREE);
                  }
                }
              }
            }
          }
        ENDIF

        moved = true;
        break;
        }

      case FldLiteralA:
      case FldAlphanumeric:
        {
        // Alphanumeric to float is inherently slow.  Send it off to the library
        break;
        }

      default:
        cbl_internal_error("In mh_dest_is_float(%s to %s), the "
                           "move of %s to %s hasn't been implemented",
              sourceref.field->name,
              destref.field->name,
              cbl_field_type_str(sourceref.field->type),
              cbl_field_type_str(destref.field->type));
        break;
      }
    }
  return moved;
  }

static void
picky_memset(tree &dest_p, unsigned char value, size_t length)
  {
  if( length )
    {
    tree dest_ep = gg_define_variable(TREE_TYPE(dest_p));
    gg_assign(dest_ep,
              gg_add( dest_p,
                      build_int_cst_type(SIZE_T, length)));
    WHILE( dest_p, lt_op, dest_ep )
      {
      gg_assign(gg_indirect(dest_p),
                build_int_cst_type(UCHAR, value));
      gg_increment(dest_p);
      }
      WEND
    }
  }

static void
picky_memcpy(tree &dest_p, tree &source_p, size_t length)
  {
  if( length )
    {
    tree dest_ep = gg_define_variable(TREE_TYPE(dest_p));
    gg_assign(dest_ep,
              gg_add( dest_p,
                      build_int_cst_type(SIZE_T, length)));
    WHILE( dest_p, lt_op, dest_ep )
      {
      gg_assign(gg_indirect(dest_p), gg_indirect(source_p));
      gg_increment(dest_p);
      gg_increment(source_p);
      }
      WEND
    }
  }

static bool
mh_numeric_display( cbl_refer_t &destref,
                    cbl_refer_t &sourceref,
                    TREEPLET    &tsource,
                    tree size_error)
  {
  bool moved = false;

  if(     destref.field->type   == FldNumericDisplay
      &&  sourceref.field->type == FldNumericDisplay
      &&  !(destref.field->attr   & scaled_e)
      &&  !(sourceref.field->attr & scaled_e) )
    {
    Analyze();
    // I believe that there are 225 pathways through the following code.  That's
    // because there are five different valid combination of signable_e,
    // separate_e, and leading_e.  There are three possibilities for
    // sender/receiver rdigits (too many, too few, and just right), and the same
    // for ldigits.  5 * 5 * 3 * 3 = 225.

    // Fasten your seat belts.

    // In order to simplify processing of a signable internal sender, we are
    // going to pick up the sign byte and temporarily turn off the sign bit in
    // the source data.  At the end, we will restore that value.  This
    // reflexively makes me a bit nervous (it isn't, for example, thread-safe),
    // but it makes life easier.

    static tree source_sign_loc  = gg_define_variable(UCHAR_P, "..mhnd_sign_loc", vs_file_static);
    static tree source_sign_byte = gg_define_variable(UCHAR,   "..mhnd_sign_byte", vs_file_static);
    static tree dest_p    = gg_define_variable(UCHAR_P, "..mhnd_dest", vs_file_static); // The destination data pointer
    static tree source_p  = gg_define_variable(UCHAR_P, "..mhnd_source", vs_file_static); // The source data pointer
    static tree source_ep = gg_define_variable(UCHAR_P, "..mhnd_source_e", vs_file_static); // When we need an end pointer

    gg_assign(dest_p,   qualified_data_dest(destref));
    gg_assign(source_p, gg_add(member(sourceref.field, "data"),
                               tsource.offset));

    if( sourceref.field->attr & signable_e )
      {
      // The source is signable

      if( !(sourceref.field->attr & leading_e) )
        {
        // The sign location is trailing.  Whether separate or not, the location
        // is the final byte of the data:
        gg_assign(source_sign_loc, gg_add(member( sourceref.field->var_decl_node, "data"),
                                          tsource.offset)),
        gg_assign(source_sign_loc,
                  gg_add(source_sign_loc,
                         build_int_cst_type(SIZE_T,
                                            sourceref.field->data.capacity-1)));
        if( (sourceref.field->attr & separate_e) )
          {
          // We have trailing separate
          }
        else
          {
          // We have trailing internal
          }
        }
      else
        {
        // The source sign location is in the leading position.
        gg_assign(source_sign_loc,
                  gg_add(member(sourceref.field->var_decl_node, "data"),
                         tsource.offset));
        if( (sourceref.field->attr & separate_e) )
          {
          // We have leading separate, so the first actual digit is at
          // source_p+1.
          gg_increment(source_p);
          }
        else
          {
          // We have leading internal
          }
        }
      // Pick up the byte that contains the sign data, whether internal or
      // external:
      gg_assign(source_sign_byte, gg_indirect(source_sign_loc));

      if( !(sourceref.field->attr & separate_e) )
        {
        // This is signable and internal, so we want to turn off the sign bit
        // in the original source data
        if( internal_codeset_is_ebcdic() )
          {
          gg_assign(gg_indirect(source_sign_loc),
                    gg_bitwise_or(source_sign_byte,
                                  build_int_cst_type( UCHAR,
                                                      NUMERIC_DISPLAY_SIGN_BIT)));
          }
        else
          {
          gg_assign(gg_indirect(source_sign_loc),
                    gg_bitwise_and( source_sign_byte,
                                    build_int_cst_type( UCHAR,
                                                       ~NUMERIC_DISPLAY_SIGN_BIT)));
          }
        }
      }
    else
      {
      // The number is unsigned, so do nothing.
      }

    // Let the shenanigans begin.

    // We are now ready to output the very first byte.

    // The first thing to do is see if we need to output a leading sign
    // character
    if(    (destref.field->attr & signable_e)
        && (destref.field->attr & leading_e)
        && (destref.field->attr & separate_e) )
      {
      // The output is signed, separate, and leading, so the first character
      // needs to be either '+' or '-'
      if( (sourceref.field->attr & separate_e) )
        {
        // The source is signable/separate
        // Oooh.  Shiny.  We already have that character.
        gg_assign(gg_indirect(dest_p), source_sign_byte);
        }
      else
        {
        // The source is internal.  Not that up above we set source_sign_byte
        // even for source values that aren't signable
        if( internal_codeset_is_ebcdic() )
          {
          // We are working in EBCDIC
          if( sourceref.field->attr & signable_e )
            {
            IF( gg_bitwise_and( source_sign_byte,
                                build_int_cst_type( UCHAR,
                                                    NUMERIC_DISPLAY_SIGN_BIT)),
                eq_op,
                build_int_cst_type( UCHAR, 0) )
              {
              // The source was negative
              gg_assign(gg_indirect(dest_p),
                        build_int_cst_type( UCHAR, EBCDIC_MINUS));

              }
            ELSE
              {
              // The source was positive
              gg_assign(gg_indirect(dest_p),
                        build_int_cst_type( UCHAR, EBCDIC_PLUS));
              }
              ENDIF
            }
          else
            {
            // The source is not signable, so the result is positive
            gg_assign(gg_indirect(dest_p),
                      build_int_cst_type( UCHAR, EBCDIC_PLUS));
            }
          }
        else
          {
          // We are working in ASCII
          if( sourceref.field->attr & signable_e )
            {
            IF( gg_bitwise_and( source_sign_byte,
                                build_int_cst_type( UCHAR,
                                                    NUMERIC_DISPLAY_SIGN_BIT)),
                ne_op,
                build_int_cst_type( UCHAR, 0) )
              {
              // The source was negative
              gg_assign(gg_indirect(dest_p),
                        build_int_cst_type( UCHAR, '-'));

              }
            ELSE
              {
              // The source was positive
              gg_assign(gg_indirect(dest_p),
                        build_int_cst_type( UCHAR, '+'));
              }
              ENDIF
            }
          else
            {
            // The source is not signable, so the result is positive
            gg_assign(gg_indirect(dest_p),
                      build_int_cst_type( UCHAR, '+'));
            }
          }
        }
      gg_increment(dest_p);
      }

    // We have the leading '+' or '-', assuming one is needed.  We can
    // now start outputting the digits to the left of the decimal place

    int dest_ldigits   = (int)destref.field->data.digits
                              - destref.field->data.rdigits;
    int source_ldigits = (int)sourceref.field->data.digits
                              - sourceref.field->data.rdigits;

    int digit_count = 0;

    if( dest_ldigits > source_ldigits )
      {
      // The destination has more ldigits than the source, and needs some
      // leading zeroes:
      picky_memset( dest_p,
                    internal_codeset_is_ebcdic() ?
                                EBCDIC_ZERO : '0' ,
                    dest_ldigits - source_ldigits);
      // With the leading zeros set, copy over the ldigits:
      digit_count = source_ldigits;
      }
    else if( dest_ldigits == source_ldigits )
      {
      // This is the Goldilocks zone.  Everything is *just* right.
      digit_count = dest_ldigits;
      }
    else
      {
      // The destination is smaller than the source.  We have to throw away the
      // the high-order digits of the source.  If any of them are non-zero, then
      // we need to indicate a size error.
      gg_assign(source_ep,
                gg_add( source_p,
                        build_int_cst_type( SIZE_T,
                                            source_ldigits-dest_ldigits)));
      WHILE(source_p, lt_op, source_ep)
        {
        if( size_error )
          {
          IF( gg_indirect(source_p),
              ne_op,
              build_int_cst_type( UCHAR,
                                  internal_codeset_is_ebcdic() ?
                                              EBCDIC_ZERO : '0') )
            {
            set_exception_code(ec_size_truncation_e);
            gg_assign(size_error, integer_one_node);
            }
          ELSE
            ENDIF
          }
        gg_increment(source_p);
        }
        WEND

      // Having skipped over the leading digits, we are in position to move the
      // remaining digits
      digit_count = dest_ldigits;
      }

    // The ldigits are in place.  We now go the very similar exercise for the
    // rdigits:

    int dest_rdigits   = destref.field->data.rdigits;
    int source_rdigits = sourceref.field->data.rdigits;

    int trailing_zeros = 0;

    if( dest_rdigits > source_rdigits )
      {
      // The destination has more rdigits than the source

      // Copy over the available digits:
      digit_count += source_rdigits;

      // And then tack on the needed trailing zeroes:
      trailing_zeros = dest_rdigits - source_rdigits;
      }
    else if( dest_rdigits == source_rdigits )
      {
      // This is the Goldilocks zone.  Everything is *just* right.
      digit_count += dest_rdigits;
      }
    else
      {
      // The destination has fewer rdigits than the source.  We send
      // over only the necessary rdigits, discarding the ones to the right.
      digit_count += dest_rdigits;
      }

    picky_memcpy(dest_p, source_p, digit_count);
    picky_memset( dest_p,
                  internal_codeset_is_ebcdic() ?
                              EBCDIC_ZERO : '0' ,
                  trailing_zeros);

    // With the digits in place, we need to sort out what to do if the target
    // is signable:
    if( destref.field->attr & signable_e )
      {
      if(     (destref.field->attr & separate_e)
          && !(destref.field->attr & leading_e) )
        {
        // The target is separate/trailing, so we need to tack a '+'
        // or '-' character
        if( sourceref.field->attr & separate_e )
          {
          // The source was separate, so we already have what we need in t
          // source_sign_byte:
          gg_assign(gg_indirect(dest_p), source_sign_byte);
          gg_increment(dest_p);
          }
        else
          {
          // The source is either internal, or unsigned
          if( sourceref.field->attr & signable_e )
            {
            // The source is signable/internal, so we need to extract the
            // sign bit from source_sign_byte
            if( internal_codeset_is_ebcdic() )
              {
              IF( gg_bitwise_and( source_sign_byte,
                                  build_int_cst_type( UCHAR,
                                                      NUMERIC_DISPLAY_SIGN_BIT)),
                  eq_op,
                  build_int_cst_type( UCHAR, 0) )
                {
                // The source was negative
                gg_assign(gg_indirect(dest_p),
                          build_int_cst_type( UCHAR, EBCDIC_MINUS));

                }
              ELSE
                {
                // The source was positive
                gg_assign(gg_indirect(dest_p),
                          build_int_cst_type( UCHAR, EBCDIC_PLUS));
                }
                ENDIF
              }
            else
              {
              IF( gg_bitwise_and( source_sign_byte,
                                  build_int_cst_type( UCHAR,
                                                      NUMERIC_DISPLAY_SIGN_BIT)),
                  ne_op,
                  build_int_cst_type( UCHAR, 0) )
                {
                // The source was negative
                gg_assign(gg_indirect(dest_p),
                          build_int_cst_type( UCHAR, '-'));

                }
              ELSE
                {
                // The source was positive
                gg_assign(gg_indirect(dest_p),
                          build_int_cst_type( UCHAR, '+'));
                }
                ENDIF
              }
            }
          else
            {
            // The source is unsigned, so dest is positive
            gg_assign(gg_indirect(dest_p),
                      build_int_cst_type( UCHAR,
                                          internal_codeset_is_ebcdic() ?
                                          EBCDIC_PLUS : '+' ));
            }
          }
        gg_increment(dest_p);
        }
      else if( !(destref.field->attr & separate_e) )
        {
        // The destination is signed/internal
        if( destref.field->attr & leading_e )
          {
          // The sign bit goes into the first byte:
          gg_assign(dest_p, qualified_data_dest(destref));
          }
        else
          {
          // The sign bit goes into the last byte:
          gg_decrement(dest_p);
          }
        if( sourceref.field->attr & signable_e )
          {
          if( sourceref.field->attr & separate_e )
            {
            // The source is separate, so source_sign_byte is '+' or '-'
            IF( source_sign_byte,
                eq_op,
                build_int_cst_type(UCHAR,
                                   internal_codeset_is_ebcdic() ?
                                        EBCDIC_MINUS : '-') )
              {
              // The source is negative, so turn the ASCII bit on
              if( !internal_codeset_is_ebcdic() )
                {
                gg_assign(gg_indirect(dest_p),
                          gg_bitwise_or(gg_indirect(dest_p),
                                        build_int_cst_type(
                                                  UCHAR,
                                                  NUMERIC_DISPLAY_SIGN_BIT)));

                }
              else
                {
                // It's ebcdic, so turn the sign bit OFF
                gg_assign(gg_indirect(dest_p),
                          gg_bitwise_and(gg_indirect(dest_p),
                                        build_int_cst_type(
                                                  UCHAR,
                                                  ~NUMERIC_DISPLAY_SIGN_BIT)));
                }
              }
            ELSE
              {
              // The source is positive, so turn the EBCDIC bit ON:
              if( internal_codeset_is_ebcdic() )
                {
                gg_assign(gg_indirect(dest_p),
                          gg_bitwise_or(gg_indirect(dest_p),
                                        build_int_cst_type(
                                                  UCHAR,
                                                  NUMERIC_DISPLAY_SIGN_BIT)));
                }
              }
              ENDIF
            }
          else
            {
            // The source is signable/internal, so the sign bit is in
            // source_sign_byte. Whatever it is, it has to go into dest_p:
            if( internal_codeset_is_ebcdic()  )
              {
              // This is EBCDIC, so if the source_sign_byte bit is LOW, we
              // clear that bit in dest_p high.
              IF( gg_bitwise_and( source_sign_byte,
                                  build_int_cst_type(
                                               UCHAR,
                                               NUMERIC_DISPLAY_SIGN_BIT)),
                  eq_op,
                  build_int_cst_type(UCHAR, 0) )
                {
                // The source was negative, so make the dest negative
                gg_assign(gg_indirect(dest_p),
                          gg_bitwise_and(gg_indirect(dest_p),
                                         build_int_cst_type(
                                                  UCHAR,
                                                  ~NUMERIC_DISPLAY_SIGN_BIT)));
                }
              ELSE
                ENDIF
              }
            else
              {
              // This is ASCII, so if the source_sign_byte bit is high, we
              // set that bit in dest_p high.
              IF( gg_bitwise_and( source_sign_byte,
                                  build_int_cst_type(
                                               UCHAR,
                                               NUMERIC_DISPLAY_SIGN_BIT)),
                  ne_op,
                  build_int_cst_type(UCHAR, 0) )
                {
                // The source was negative, so make the dest negative
                gg_assign(gg_indirect(dest_p),
                          gg_bitwise_or(gg_indirect(dest_p),
                                        build_int_cst_type(
                                                  UCHAR,
                                                  NUMERIC_DISPLAY_SIGN_BIT)));
                }
              ELSE
                ENDIF
              }
            }
          }
        }
      }

    if(     (sourceref.field->attr & signable_e)
        && !(sourceref.field->attr & separate_e))
      {
      // The source is signable internal, so we need to restore the original
      // sign byte in the original source data:
      gg_assign(gg_indirect(source_sign_loc), source_sign_byte);
      }
    moved = true;
    }
  return moved;
  }

static bool
mh_little_endian( cbl_refer_t &destref,
                  cbl_refer_t &sourceref,
                  TREEPLET    &tsource,
                  bool check_for_error,
                  tree size_error)
  {
  bool moved = false;

  cbl_figconst_t figconst = cbl_figconst_of( sourceref.field->data.initial);

  if(     !figconst
      &&  !(destref.field->attr    & scaled_e)
      &&  !(destref.field->attr    & (intermediate_e  ))
      &&  !(sourceref.field->attr  & (intermediate_e  ))
      &&  sourceref.field->type     != FldLiteralA
      &&  sourceref.field->type     != FldAlphanumeric
      &&  sourceref.field->type     != FldNumericEdited
      &&  sourceref.field->type     != FldPacked
      &&  (     destref.field->type == FldNumericBin5
            ||  destref.field->type == FldPointer
            ||  destref.field->type == FldIndex ) )
    {
    Analyze();
    SHOW_PARSE1
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT("mh_little_endian")
      SHOW_PARSE_END
      }

    int bytes_needed = get_bytes_needed(sourceref.field);
    tree source_type = tree_type_from_size(bytes_needed,
                                           sourceref.field->attr
                                                                & signable_e) ;
    tree source = gg_define_variable(source_type);

    if( sourceref.field->type == FldFloat )
      {
      get_binary_value_from_float(source,
                                  destref,
                                  sourceref.field,
                                  tsource.offset);

      // Get binary value from float actually scales the source value to the
      // dest:: rdigits
      copy_little_endian_into_place(destref.field,
                                    refer_offset_dest(destref),
                                    source,
                                    destref.field->data.rdigits,
                                    check_for_error,
                                    size_error);
      moved = true;
      }
    else
      {
      get_binary_value( source,
                        NULL,
                        sourceref.field,
                        tsource.offset);
      copy_little_endian_into_place(destref.field,
                                    refer_offset_dest(destref),
                                    source,
                                    sourceref.field->data.rdigits,
                                    check_for_error,
                                    size_error);
      moved = true;
      }
    }
  return moved;
  }

static bool
mh_source_is_group( cbl_refer_t &destref,
                    cbl_refer_t &sourceref,
                    TREEPLET    &tsrc)
  {
  bool retval = false;
  if( sourceref.field->type == FldGroup && !(destref.field->attr & rjust_e) )
    {
    Analyze();
    // We are moving a group to a something.  The rule here is just move as
    // many bytes as you can, and, if necessary, fill with spaces
    tree tdest   = gg_add( member(destref.field->var_decl_node, "data"),
                           refer_offset_dest(destref));
    tree tsource = gg_add( member(sourceref.field->var_decl_node, "data"),
                           tsrc.offset);
    tree dbytes  = refer_size_dest(destref);
    tree sbytes  = tsrc.length;

    IF( sbytes, ge_op, dbytes )
      {
      // There are too many source bytes
      gg_memcpy(tdest, tsource, dbytes);
      }
    ELSE
      {
      // There are too-few source bytes:
      gg_memset(tdest, build_int_cst_type(INT, internal_space), dbytes);
      gg_memcpy(tdest, tsource, sbytes);
      }
    ENDIF
    retval = true;
    }
  return retval;
  }

static void
move_helper(tree size_error,        // This is an INT
            cbl_refer_t destref,
            cbl_refer_t sourceref,  // Call move_helper with this resolved.
            TREEPLET   &tsource,
            cbl_round_t rounded,
            bool check_for_error,   // True means our called wants to know about truncation errors
            bool restore_on_error
            )
  {
  Analyze();
  SHOW_PARSE1
    {
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT("move_helper()");
    }

  bool moved = false;

  if( size_error )
    {
    gg_assign(size_error, integer_zero_node);
    }

  static tree stash = gg_define_variable(UCHAR_P, "..mh_stash", vs_file_static);

  tree st_data = NULL_TREE;
  tree st_size = NULL_TREE;

  if( restore_on_error )
    {
    // We are creating a copy of the original destination in case we clobber it
    // and have to restore it because of a computational error.
    bool first_time = true;
    static size_t stash_size = 1024;
    if( first_time )
      {
      first_time = false;
      gg_assign(stash, gg_cast(UCHAR_P, gg_malloc(stash_size)));
      }
    if( stash_size < destref.field->data.capacity )
      {
      stash_size = destref.field->data.capacity;
      gg_assign(stash, gg_cast(UCHAR_P, gg_realloc(stash, stash_size)));
      }
    st_data = qualified_data_dest(destref);
    st_size = refer_size_dest(destref);
    gg_memcpy(stash,
              st_data,
              st_size);
    }

  if(     (sourceref.field->attr & (linkage_e | based_e))
      ||  (  destref.field->attr & (linkage_e | based_e)) )
    {
    //goto dont_be_clever; this will go through to the default.
    }

  if( !moved )
    {
    moved = mh_source_is_group(destref, sourceref, tsource);
    }

  if( !moved )
    {
    moved = mh_identical(destref, sourceref, tsource);
    }

  if( !moved )
    {
    moved = mh_source_is_literalN(destref,
                                  sourceref,
                                  check_for_error,
                                  rounded,
                                  size_error);
    }

  if( !moved )
    {
    moved = mh_dest_is_float( destref,
                              sourceref,
                              tsource,
                              rounded,
                              size_error);
    }

  if( !moved && rounded == truncation_e )
    {
    moved = mh_numeric_display( destref,
                                sourceref,
                                tsource,
                                size_error);
    }

  if( !moved )
    {
    moved = mh_little_endian( destref,
                              sourceref,
                              tsource,
                              restore_on_error,
                              size_error);
    }

  if( !moved && sourceref.field->type == FldLiteralA)
    {
    SHOW_PARSE1
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT("__gg__move_literala")
      }

    cbl_figconst_t figconst = cbl_figconst_of( sourceref.field->data.initial);

    if(    destref.refmod.from
        || destref.refmod.len )
      {
      // Let the move routine know to treat the destination as alphanumeric
      gg_attribute_bit_set(destref.field, refmod_e);
      }

    static char *buffer = NULL;
    static size_t buffer_size = 0;
    size_t source_length = sourceref.field->data.capacity;

    if( buffer_size < source_length )
      {
      buffer_size = source_length;
      buffer = (char *)xrealloc(buffer, buffer_size);
      }

    if( figconst )
      {
      char const_char = 0x7F;  // Head off a compiler warning about
      //                       // uninitialized variables
      switch(figconst)
        {
        case normal_value_e :
          // This is not possible, it says here in the fine print.
          abort();
          break;
        case low_value_e    :
          const_char = ascii_to_internal(__gg__low_value_character);
          break;
        case zero_value_e   :
          const_char = internal_zero;
          break;
        case space_value_e  :
          const_char = internal_space;
          break;
        case quote_value_e  :
          const_char = ascii_to_internal(__gg__quote_character);
          break;
        case high_value_e   :
          const_char = ascii_to_internal(__gg__high_value_character);
          break;
        case null_value_e:
          const_char = 0x00;
          break;
        }
      memset(buffer, const_char, source_length);
      }
     else
      {
      memset( buffer, ascii_space, source_length);
      memcpy( buffer,
              sourceref.field->data.initial,
              std::min(source_length, (size_t)sourceref.field->data.capacity) );
      for( size_t i=0; i<source_length; i++)
        {
        buffer[i] = ascii_to_internal(buffer[i]);
        }
      }

    int rounded_parameter = rounded
                            | ((sourceref.all || figconst ) ? REFER_ALL_BIT : 0);

    if( size_error )
      {
      gg_assign(size_error,
                gg_call_expr( INT,
                              "__gg__move_literala",
                              gg_get_address_of(destref.field->var_decl_node),
                              refer_offset_dest(destref),
                              refer_size_dest(destref),
                              build_int_cst_type(INT, rounded_parameter),
                              build_string_literal(source_length,
                                                   buffer),
                              build_int_cst_type( SIZE_T, source_length),
                              NULL_TREE));
      }
    else
      {
                gg_call     ( INT,
                              "__gg__move_literala",
                              gg_get_address_of(destref.field->var_decl_node),
                              refer_offset_dest(destref),
                              refer_size_dest(destref),
                              build_int_cst_type(INT, rounded_parameter),
                              build_string_literal(source_length,
                                                   buffer),
                              build_int_cst_type( SIZE_T, source_length),
                              NULL_TREE);
      }
    if(    destref.refmod.from
        || destref.refmod.len )
      {
      // Return that value to its original form
      gg_attribute_bit_clear(destref.field, refmod_e);
      }
    moved = true;
    }

  if( !moved )
    {
    SHOW_PARSE1
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT("default __gg__move")
      }

    if(    destref.refmod.from
        || destref.refmod.len
        || sourceref.refmod.from
        || sourceref.refmod.len )
      {
      // Let the move routine know to treat the destination as alphanumeric
      gg_attribute_bit_set(destref.field, refmod_e);
      }

    int nflags =   (sourceref.all      ? REFER_T_MOVE_ALL   : 0)
                 + (sourceref.addr_of  ? REFER_T_ADDRESS_OF : 0);

    if( size_error )
      {
      gg_assign(size_error,
                gg_call_expr( INT,
                              "__gg__move",
                              gg_get_address_of(destref.field->var_decl_node),
                              refer_offset_dest(destref),
                              refer_size_dest(destref),
                              tsource.pfield,
                              tsource.offset,
                              tsource.length,
                              build_int_cst_type(INT, nflags),
                              build_int_cst_type(INT, rounded),
                              NULL_TREE));
      }
    else
      {
                gg_call     ( INT,
                              "__gg__move",
                              gg_get_address_of(destref.field->var_decl_node),
                              refer_offset_dest(destref),
                              refer_size_dest(destref),
                              tsource.pfield,
                              tsource.offset,
                              tsource.length,
                              build_int_cst_type(INT, nflags),
                              build_int_cst_type(INT, rounded),
                              NULL_TREE);

      }
    if(    destref.refmod.from
        || destref.refmod.len
        || sourceref.refmod.from
        || sourceref.refmod.len )
      {
      // Return that value to its original form
      gg_attribute_bit_clear(destref.field, refmod_e);
      }

    moved = true;
    }

  if( restore_on_error )
    {
    IF(size_error, ne_op, integer_zero_node)
      {
      gg_memcpy(st_data,
                stash,
                st_size);
      }
    ELSE
      ENDIF
    }
  else
    {
    if( check_for_error )
      {
      IF(size_error, ne_op, integer_zero_node)
        {
        // We had a size error, but  there was no restore_on_error. Pointer
        // Let our lord and master know there was a truncation:
        set_exception_code(ec_size_truncation_e);
        }
      ELSE
        ENDIF
      }
    }

  SHOW_PARSE1
    {
    SHOW_PARSE_END
    }
  }

tree parser_cast_long(tree N)
  {
  return gg_cast(LONG, N);
  }

void
parser_print_long(tree N)
  {
  gg_printf("%ld", N, NULL_TREE);
  }

void
parser_print_long(const char *fmt, tree N)
  {
  // fmt should have a %ld/%lx in it
  gg_printf(fmt, N, NULL_TREE);
  }

void
parser_print_long(long N)
  {
  gg_printf("%ld", build_int_cst_type(LONG, N), NULL_TREE);
  }

void
parser_print_long(const char *fmt, long N)
  {
  // fmt should have a %ld/%lx in it
  gg_printf(fmt, build_int_cst_type(LONG, N), NULL_TREE);
  }

void
parser_print_string(const char *ach)
  {
  gg_printf("%s", gg_string_literal(ach), NULL_TREE);
  }

void
parser_print_string(const char *fmt, const char *ach)
  {
  // fmt should have a %s in it
  gg_printf(fmt, gg_string_literal(ach), NULL_TREE);
  }

REAL_VALUE_TYPE
real_powi10 (uint32_t x)
{
  REAL_VALUE_TYPE ten, pow10;
  real_from_integer (&ten, TYPE_MODE (FLOAT128), 10, SIGNED);
  real_powi (&pow10, TYPE_MODE (FLOAT128), &ten, x);
  return pow10;
}

char *
binary_initial_from_float128(cbl_field_t *field, int rdigits,
                             REAL_VALUE_TYPE value)
  {
  // This routine returns an xmalloced buffer designed to replace the
  // data.initial member of the incoming field
  char *retval = NULL;

  // We need to adjust value so that it has no decimal places
  if( rdigits )
    {
      REAL_VALUE_TYPE pow10 = real_powi10 (rdigits);
      real_arithmetic (&value, MULT_EXPR, &value, &pow10);
      real_convert (&value, TYPE_MODE (float128_type_node), &value);
    }
  // We need to make sure that the resulting string will fit into
  // a number with 'digits' digits

  // Keep in mind that pure binary types, like BINARY-CHAR, have no digits
  if( field->data.digits )
    {
      REAL_VALUE_TYPE pow10 = real_powi10 (field->data.digits);
      mpfr_t m0, m1;

      mpfr_inits2 (REAL_MODE_FORMAT (TYPE_MODE (float128_type_node))->p,
                   m0, m1, NULL);
      mpfr_from_real (m0, &value, MPFR_RNDN);
      mpfr_from_real (m1, &pow10, MPFR_RNDN);
      mpfr_clear_flags ();
      mpfr_fmod (m0, m0, m1, MPFR_RNDN);
      real_from_mpfr (&value, m0,
                      REAL_MODE_FORMAT (TYPE_MODE (float128_type_node)),
                      MPFR_RNDN);
      real_convert (&value, TYPE_MODE (float128_type_node), &value);
      mpfr_clears (m0, m1, NULL);
    }

  real_roundeven (&value, TYPE_MODE (float128_type_node), &value);

  bool fail = false;
  FIXED_WIDE_INT(128) i
    = FIXED_WIDE_INT(128)::from (real_to_integer (&value, &fail, 128), SIGNED);

  retval = (char *)xmalloc(field->data.capacity);
  switch(field->data.capacity)
    {
      tree type;
    case 1:
    case 2:
    case 4:
    case 8:
    case 16:
      type = build_nonstandard_integer_type (field->data.capacity
					     * BITS_PER_UNIT, 0);
      native_encode_wide_int (type, i, (unsigned char *)retval,
			      field->data.capacity);
      break;
    default:
      fprintf(stderr,
              "Trouble in initial_from_float128 at %s() %s:%d\n",
              __func__,
              __FILE__,
              __LINE__);
      abort();
      break;
    }

  return retval;
  }


static void
digits_from_float128(char *retval, cbl_field_t *field, size_t width, int rdigits, REAL_VALUE_TYPE value)
  {
  char ach[128];

  // We need to adjust value so that it has no decimal places
  if( rdigits )
    {
      REAL_VALUE_TYPE pow10 = real_powi10 (rdigits);
      real_arithmetic (&value, MULT_EXPR, &value, &pow10);
    }
  // We need to make sure that the resulting string will fit into
  // a number with 'digits' digits
  REAL_VALUE_TYPE pow10 = real_powi10 (field->data.digits);
  mpfr_t m0, m1;

  mpfr_inits2 (FLOAT_MODE_FORMAT (TYPE_MODE (float128_type_node))->p, m0, m1,
               NULL);
  mpfr_from_real (m0, &value, MPFR_RNDN);
  mpfr_from_real (m1, &pow10, MPFR_RNDN);
  mpfr_clear_flags ();
  mpfr_fmod (m0, m0, m1, MPFR_RNDN);
  real_from_mpfr (&value, m0,
                  REAL_MODE_FORMAT (TYPE_MODE (float128_type_node)),
                  MPFR_RNDN);
  real_convert (&value, TYPE_MODE (float128_type_node), &value);
  mpfr_clears (m0, m1, NULL);
  real_roundeven (&value, TYPE_MODE (float128_type_node), &value);

  bool fail = false;
  FIXED_WIDE_INT(128) i
    = FIXED_WIDE_INT(128)::from (real_to_integer (&value, &fail, 128), SIGNED);

  // We convert it to a integer string of digits:
  print_dec (i, ach, SIGNED);

  //fprintf(stderr, "digits_from_float128() %s %f %s ", field->name, (double)value, ach);

  gcc_assert( strlen(ach) <= field->data.digits );
  if( strlen(ach) < width )
    {
    memset(retval, '0', width-strlen(ach) );
    }
  strcpy(retval + (width-strlen(ach)), ach);
  }

static char *
initial_from_float128(cbl_field_t *field)
  {
  Analyze();
  // This routine returns an xmalloced buffer that is intended to replace the
  // data.initial member of the incoming field.

  //fprintf(stderr, "initial_from_float128 %s\n", field->name);

  char *retval = NULL;
  int rdigits;

  // Let's handle the possibility of a figurative constant
  cbl_figconst_t figconst = cbl_figconst_of( field->data.initial);
  //cbl_figconst_t figconst = (cbl_figconst_t)(field->attr & FIGCONST_MASK);
  if( figconst )
    {
    int const_char = 0xFF;  // Head off a compiler warning about uninitialized
    //                      // variables
    switch(figconst)
      {
      case normal_value_e :
        // This really should never happen because normal_value_e is zero
        abort();
        break;
      case low_value_e    :
        const_char = ascii_to_internal(__gg__low_value_character);
        break;
      case zero_value_e   :
        const_char = internal_zero;
        break;
      case space_value_e  :
        const_char = internal_space;
        break;
      case quote_value_e  :
        const_char = ascii_to_internal(__gg__quote_character);
        break;
      case high_value_e   :
        if( __gg__high_value_character == DEGENERATE_HIGH_VALUE )
          {
          const_char = __gg__high_value_character;
          }
        else
          {
          const_char = ascii_to_internal(__gg__high_value_character);
          }
        break;
      case null_value_e:
        break;
      }
    bool set_return = figconst != zero_value_e;
    if( !set_return )
      {
      // The figconst is zero
      switch(field->type)
        {
        case FldGroup:
        case FldAlphanumeric:
          set_return = true;
          break;

        default:
          break;
        }
      }
    if( set_return )
      {
      retval = (char *)xmalloc(field->data.capacity);
      memset(retval, const_char, field->data.capacity);
      return retval;
      }
    }

  // ???  Refactoring the cases below that do not need 'value' would
  // make this less ugly
  REAL_VALUE_TYPE value;
  if( field->data.etc_type == cbl_field_data_t::value_e )
    value = TREE_REAL_CST (field->data.value_of ());

  // There is always the infuriating possibility of a P-scaled number
  if( field->attr & scaled_e )
    {
    rdigits = 0;
    if( field->data.rdigits >= 0 )
      {
      // Suppose our PIC is PPPPPP999, meaning that field->digits
      // is 3, and field->rdigits is 6.

      // Our result has no decimal places, and we have to multiply the value
      // by 10**9 to get the significant bdigits where they belong.

      REAL_VALUE_TYPE pow10
        = real_powi10 (field->data.digits + field->data.rdigits);
      real_arithmetic (&value, MULT_EXPR, &value, &pow10);
      }
    else
      {
      // Suppose our target is 999PPPPPP, so there is a ->digits
      // of 3 and field->rdigits of -6.

      // If our caller gave us 123000000, we need to divide
      // it by 1000000 to line up the 123 with where we want it to go:

      REAL_VALUE_TYPE pow10 = real_powi10 (-field->data.rdigits);
      real_arithmetic (&value, RDIV_EXPR, &value, &pow10);
      }
    // Either way, we now have everything aligned for the remainder of the
    // processing to work:
    }
  else
    {
    // Not P-scaled
    rdigits = field->data.rdigits;
    }

  switch(field->type)
    {
    case FldNumericBin5:
    case FldIndex:
      retval = binary_initial_from_float128(field, rdigits, value);
      break;

    case FldNumericBinary:
      {
      retval = binary_initial_from_float128(field, rdigits, value);
      size_t left = 0;
      size_t right = field->data.capacity - 1;
      while(left < right)
        {
        std::swap(retval[left++], retval[right--]);
        }
      break;
      }

    case FldNumericDisplay:
      {
      retval = (char *)xmalloc(field->data.capacity);
      char *pretval = retval;
      char ach[128];

      bool negative;
      if( real_isneg (&value) )
        {
          negative = true;
          value = real_value_negate (&value);
        }
      else
        {
          negative = false;
        }

      digits_from_float128(ach, field, field->data.digits, rdigits, value);

      char *digits = ach;
      if(    (field->attr & signable_e)
          && (field->attr & separate_e)
          && (field->attr & leading_e ) )
        {
        if( negative )
          {
          *pretval++ = internal_minus;
          }
        else
          {
          *pretval++ = internal_plus;
          }
        }
      for(size_t i=0; i<field->data.digits; i++)
        {
        *pretval++ = internal_zero + ((*digits++) & 0x0F);
        }
      if(     (field->attr & signable_e)
          &&  (field->attr & separate_e)
          && !(field->attr & leading_e ) )
        {
        if( negative )
          {
          *pretval++ = internal_minus;
          }
        else
          {
          *pretval++ = internal_plus;
          }
        }
      if(     (field->attr & signable_e)
          && !(field->attr & separate_e)
          &&  negative)
        {
        if( field->attr & leading_e )
          {
          if( internal_is_ebcdic )
            {
            retval[0] &= ~NUMERIC_DISPLAY_SIGN_BIT;
            }
          else
            {
            retval[0] |=  NUMERIC_DISPLAY_SIGN_BIT;
            }
          }
        else
          {
          if( internal_is_ebcdic )
            {
            pretval[-1] &= ~NUMERIC_DISPLAY_SIGN_BIT;
            }
          else
            {
            pretval[-1] |=  NUMERIC_DISPLAY_SIGN_BIT;
            }
          }
        }
      break;
      }

    case FldPacked:
      {
      retval = (char *)xmalloc(field->data.capacity);
      char *pretval = retval;
      char ach[128];

      bool negative;
      if( real_isneg (&value) )
        {
          negative = true;
          value = real_value_negate (&value);
        }
      else
        {
          negative = false;
        }

      // For COMP-6 (flagged by separate_e), the number of required digits is
      // twice the capacity.

      // For COMP-3, the number of digits is 2*capacity minus 1, because the
      // the final "digit" is a sign nybble.

      size_t ndigits =   (field->attr & separate_e)
                       ? field->data.capacity * 2
                       : field->data.capacity * 2 - 1;
      digits_from_float128(ach, field, ndigits, rdigits, value);

      char *digits = ach;
      for(size_t i=0; i<ndigits; i++)
        {
        if( !(i & 0x01) )
          {
          *pretval    = ((*digits++) & 0x0F)<<4;;
          }
        else
          {
          *pretval++ += (*digits++) & 0x0F;
          }
        }
      if( !(field->attr & separate_e) )
        {
        // This is COMP-3, so put in a sign nybble
        if( (field->attr & signable_e) )
          {
          if( negative )
            {
            *pretval++ += 0x0D;   // Means signable and negative
            }
          else
            {
            *pretval++ += 0x0C;   // Means signable and non-negative
            }
          }
        else
          {
          *pretval++ += 0x0F;     // Means not signable
          }
        }
      break;
      }

    case FldGroup:
    case FldAlphanumeric:
    case FldLiteralA:
    case FldAlphaEdited:
      {
      if( field->data.initial )
        {
        retval = (char *)xmalloc(field->data.capacity+1);
        if( field->attr & hex_encoded_e)
          {
          memcpy(retval, field->data.initial, field->data.capacity);
          }
        else
          {
          size_t buffer_size = 0;
          size_t length = (size_t)field->data.capacity;
          memset(retval, internal_space, length);
          raw_to_internal(&retval, &buffer_size, field->data.initial, length);
          if( strlen(field->data.initial) < length )
            {
            // If this is true, then the initial string must've been Z'xyz'
            retval[strlen(field->data.initial)] = '\0';
            }
          }
        retval[field->data.capacity] = '\0';
        }
      break;
      }

    case FldNumericEdited:
      {
      retval = (char *)xmalloc(field->data.capacity+1);
      if( field->data.initial && field->attr & quoted_e )
        {
        if( field->attr & quoted_e )
          {
          // What the programmer says the value is, the value becomes, no
          // matter how wrong it might be.
          size_t length = std::min( (size_t)field->data.capacity,
                                    strlen(field->data.initial));
          for(size_t i=0; i<length; i++)
            {
            retval[i] = ascii_to_internal(field->data.initial[i]);
            }
          if( length < (size_t)field->data.capacity )
            {
            memset( retval+length,
                    internal_space,
                    (size_t)field->data.capacity - length);
            }
          }
        }
      else
        {
        // It's not a quoted string, so we use data.value:
        bool negative;
        if( real_isneg (&value) )
          {
          negative = true;
          value = real_value_negate (&value);
          }
        else
          {
          negative = false;
          }

        char ach[128];
        memset(ach, 0, sizeof(ach));
        memset(retval, 0, field->data.capacity);
        size_t ndigits = field->data.capacity;

        if( (field->attr & blank_zero_e) && real_iszero (&value) )
          {
          memset(retval, internal_space, field->data.capacity);
          }
        else
          {
          digits_from_float128(ach, field, ndigits, rdigits, value);
          /* ???  This resides in libgcobol valconv.cc.  */
          __gg__string_to_numeric_edited( retval,
                                          ach,
                                          field->data.rdigits,
                                          negative,
                                          field->data.picture);
          }
        }
      break;
      }

    case FldFloat:
      {
      retval = (char *)xmalloc(field->data.capacity);
      switch( field->data.capacity )
        {
        case 4:
          value = real_value_truncate (TYPE_MODE (FLOAT), value);
          native_encode_real (SCALAR_FLOAT_TYPE_MODE (FLOAT), &value,
			      (unsigned char *)retval, 4, 0);
          break;
        case 8:
          value = real_value_truncate (TYPE_MODE (DOUBLE), value);
          native_encode_real (SCALAR_FLOAT_TYPE_MODE (DOUBLE), &value,
			      (unsigned char *)retval, 8, 0);
          break;
        case 16:
          value = real_value_truncate (TYPE_MODE (FLOAT128), value);
          native_encode_real (SCALAR_FLOAT_TYPE_MODE (FLOAT128), &value,
			      (unsigned char *)retval, 16, 0);
          break;
        }
      break;
      }

    case FldLiteralN:
      {
      break;
      }

    default:
      break;
    }
  return retval;
  }

static void
actually_create_the_static_field( cbl_field_t *new_var,
                                  tree data_area,
                                  size_t length_of_initial_string,
                                  const char *new_initial,
                                  tree immediate_parent,
                                  tree new_var_decl)
  {
  tree constr = make_node(CONSTRUCTOR);
  TREE_TYPE(constr) = cblc_field_type_node;
  TREE_STATIC(constr)    = 1;
  TREE_CONSTANT(constr)  = 1;

  tree next_field = TYPE_FIELDS(cblc_field_type_node);
  // We are going to create the constructors by walking the linked
  // list of FIELD_DECLs.  We must do it in the same order as the
  // structure creation code in create_cblc_field_t()

  //  UCHAR_P, "data",
  CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr),
                          next_field,
                          data_area );
  next_field = TREE_CHAIN(next_field);

  //  SIZE_T,  "capacity",
  CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr),
                          next_field,
                          build_int_cst_type( SIZE_T,
                                              new_var->data.capacity) );
  next_field = TREE_CHAIN(next_field);

  //  SIZE_T,  "allocated",
  if( data_area != null_pointer_node )
    {
    CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr),
                            next_field,
                            build_int_cst_type( SIZE_T,
                                                new_var->data.capacity) );
    }
  else
    {
    CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr),
                            next_field,
                            build_int_cst_type( SIZE_T,
                                                0) );
    }

  next_field = TREE_CHAIN(next_field);

  //  SIZE_T,  "offset",

  if( new_var->type == FldAlphanumeric &&
      new_var->attr & intermediate_e )
    {
    // This is in support of FUNCTION TRIM.  That function can make the capacity
    // of the intermediate target smaller so that TRIM("abc   ") returns
    // "abc".  By putting the capacity here for such variables, we have a
    // mechanism for restoring it the capacity to the original.
    CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr),
                            next_field,
                            build_int_cst_type(SIZE_T, new_var->data.capacity));
    }
  else
    {
    CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr),
                            next_field,
                            build_int_cst_type(SIZE_T, new_var->offset) );
    }

  next_field = TREE_CHAIN(next_field);

  //  CHAR_P,  "name",
  CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr),
                          next_field,
                          gg_string_literal(new_var->name) );
  next_field = TREE_CHAIN(next_field);

  //  CHAR_P,  "picture",
  CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr),
                          next_field,
                          gg_string_literal(new_var->data.picture) );
  next_field = TREE_CHAIN(next_field);

  //  CHAR_P,  "initial",
  if( length_of_initial_string == 0 )
    {
    CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr),
                            next_field,
                            null_pointer_node );
    }
  else
    {
    CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr),
                            next_field,
                            build_string_literal(length_of_initial_string, new_initial) );
    }
    next_field = TREE_CHAIN(next_field);

  //  CHAR_P,  "parent",
  CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr),
                          next_field,
                          immediate_parent ? gg_get_address_of(immediate_parent) : null_pointer_node );
  next_field = TREE_CHAIN(next_field);

  //  SIZE_T,     "occurs_lower",
  CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr),
                          next_field,
                          build_int_cst_type(SIZE_T, new_var->occurs.bounds.lower) );
  next_field = TREE_CHAIN(next_field);

  //  SIZE_T,     "occurs_upper");
  CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr),
                          next_field,
                          build_int_cst_type(SIZE_T, new_var->occurs.bounds.upper) );
  next_field = TREE_CHAIN(next_field);

  //  SIZE_T,     "attr",
  CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr),
                          next_field,
                          build_int_cst_type(SIZE_T, new_var->attr) );
  next_field = TREE_CHAIN(next_field);

  //  SCHAR,     "type",
  CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr),
                          next_field,
                          build_int_cst_type(SCHAR, new_var->type) );
  next_field = TREE_CHAIN(next_field);

  //  SCHAR,     "level",
  CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr),
                          next_field,
                          build_int_cst_type(SCHAR, new_var->level) );
  next_field = TREE_CHAIN(next_field);

  //  SCHAR,     "digits",
  CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr),
                          next_field,
                          build_int_cst_type(SCHAR, new_var->data.digits) );
  next_field = TREE_CHAIN(next_field);

  //  SCHAR,     "rdigits",
  CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr),
                          next_field,
                          build_int_cst_type(SCHAR, new_var->data.rdigits) );
  next_field = TREE_CHAIN(next_field);

  DECL_INITIAL(new_var_decl) = constr;
  }

static void
psa_global(cbl_field_t *new_var)
  {
  char *mname = cobol_name_mangler(new_var->name);
  char ach[2*sizeof(cbl_name_t)];
  sprintf(ach, "__gg__%s", mname);
  free(mname);

  if( strcmp(new_var->name, "_VERY_TRUE") == 0 )
    {
    new_var->var_decl_node = boolean_true_node;
    return;
    }
  if( strcmp(new_var->name, "_VERY_FALSE") == 0 )
    {
    new_var->var_decl_node = boolean_false_node;
    return;
    }

  // global variables already have a cblc_field_t defined in constants.cc

  strcpy(ach, "__gg__");
  strcat(ach, new_var->name);
  for(size_t i=0; i<strlen(ach); i++)
    {
    ach[i] = _tolower(ach[i]);
    if(ach[i] == '-')
      {
      ach[i] = '_';
      }
    }

  if( strcmp(new_var->name, "RETURN-CODE") == 0 )
    {
    strcpy(ach, "__gg__return_code");
    }

  if( strcmp(new_var->name, "UPSI-0") == 0 )
    {
    strcpy(ach, "__gg__upsi");
    }

  new_var->var_decl_node = gg_declare_variable(cblc_field_type_node, ach, NULL, vs_external_reference);

  // global variables already have a .data area defined.  We can find that
  // variable from the new_var->name.  It's lower-case, with hyphens
  // converted to underscores
  strcpy(ach, "__gg__data_");
  strcat(ach, new_var->name);
  for(size_t i=0; i<strlen(ach); i++)
    {
    ach[i] = _tolower(ach[i]);
    if(ach[i] == '-')
      {
      ach[i] = '_';
      }
    }
  new_var->data_decl_node = gg_declare_variable(UCHAR, ach, NULL, vs_external_reference);
  }

static tree
psa_new_var_decl(cbl_field_t *new_var, const char *external_record_base)
  {
  // This routine creates the VAR_DECL for the cblc_field_t that we are about
  // to statically create.
  tree new_var_decl;

  if( *external_record_base )
    {
    char ach[257];
    strcpy(ach, "_");
    strcat(ach, external_record_base);
    strcat(ach, "_ra");  // For "Record Area"
    new_var_decl = gg_define_variable(  cblc_field_type_node,
                                        ach,
                                        vs_external);
    SET_DECL_MODE(new_var_decl, BLKmode);
    }
  else
    {
    size_t our_index = new_var->our_index;

    // During the early stages of implementing cbl_field_t::our_index, there
    // were execution paths in parse.y and parser.cc that resulted in our_index
    // not being set.  I hereby try to use field_index() to find the index
    // of this field to resolve those.  I note that field_index does a linear
    // search of the symbols[] table to find that index.  That's why I don't
    // use it routinely; it results in O(N^squared) computational complexity
    // to do a linear search of the symbol table for each symbol

    if(   !our_index
          && new_var->type != FldLiteralN
          && !(new_var->attr & intermediate_e))
      {
      our_index = field_index(new_var);
      if( our_index == (size_t)-1 )
        {
        // Hmm.  Couldn't find it.  Seems odd.
        our_index = 0;
        }
      }

    char base_name[257];
    char id_string[32] = "";

    if( new_var->attr & external_e )
      {
      // For external variables, just stick with the original name
      sprintf(base_name, "%s_cblc_field", new_var->name);
      }
    else
      {
      if(    our_index
          && new_var->parent
          && symbol_at(new_var->parent)->type == SymField )
        {
        // We have a parent that is a field
        sprintf(id_string, ".%ld_%ld", our_index, new_var->parent);
        }
      else
        {
        // The parent is zero, so it'll be implied:
        sprintf(id_string, ".%ld", our_index);
        }

      if(strcasecmp(new_var->name, "filler") == 0)
        {
        // Multiple "fillers" can have the same parent, so we use filler_count
        // to distinguish them.  We also prepend an underscore, so that
        // the user can't trip us up by creating their *own* cobol variable
        // named "FILLER-1"
        static int filler_count = 1;
        sprintf(base_name, "_filler_%d", filler_count++);
        }
      else if( strlen(new_var->name) == 0 )
        {
        // This can happen.
        static int empty_count = 1;
        sprintf(base_name,
                "_%s_%d",
                cbl_field_type_str(new_var->type),
                empty_count++);
        }
      else if( new_var->attr & intermediate_e )
        {
        static int inter_count = 1;
        sprintf(base_name,
                "_%s_%s_%d",
                "intermediate",
                new_var->name,
                inter_count++);
        }
      else
        {
        strcpy(base_name, new_var->name);
        }
      strcat(base_name, id_string);
      }

    if( new_var->attr & external_e )
      {
      //fprintf(stderr, "external_e base name is %s\n", base_name);
      new_var_decl = gg_define_variable(  cblc_field_type_node,
                                          base_name,
                                          vs_external);
      SET_DECL_MODE(new_var_decl, BLKmode);
      }
    else if( new_var->attr & (intermediate_e)
              && new_var->type != FldLiteralA
              && new_var->type != FldLiteralN )
      {
//      new_var_decl = gg_define_variable(  cblc_field_type_node,
//                                          base_name,
//                                          vs_static);
      new_var_decl = gg_define_variable(  cblc_field_type_node,
                                          base_name,
                                          vs_stack);
      SET_DECL_MODE(new_var_decl, BLKmode);
      }
    else
      {
      new_var_decl = gg_define_variable(  cblc_field_type_node,
                                          base_name,
                                          vs_static);
      SET_DECL_MODE(new_var_decl, BLKmode);
      }
    }
  return new_var_decl;
  }

#if 1
static void
psa_FldLiteralA(struct cbl_field_t *field )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD(" ", field)
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }

  // We are constructing a completely static constant structure.  We know the
  // capacity.  We'll create it from the data.initial.  The cblc_field_t:data
  // will be an ASCII/EBCDIC copy of the .initial data. The .initial will be
  // left as ASCII.  The var_decl_node will be an ordinary cblc_field_t, which
  // means that at this point in time, a FldLiteralA can be used anywhere a
  // FldGroup or FldAlphanumeric can be used.  We are counting on the parser
  // not allowing a FldLiteralA to be a left-hand-side variable.

  // First make room
  static size_t buffer_size = 1024;
  static char *buffer = (char *)xmalloc(buffer_size);
  if( buffer_size < field->data.capacity+1 )
    {
    buffer_size = field->data.capacity+1;
    buffer = (char *)xrealloc(buffer, buffer_size);
    }

  cbl_figconst_t figconst = cbl_figconst_of( field->data.initial );
  gcc_assert(figconst == normal_value_e);

  if( internal_codeset_is_ebcdic() )
    {
    for( size_t i=0; i<field->data.capacity; i++ )
      {
      buffer[i] = ascii_to_internal(field->data.initial[i]);
      }
    }
  else
    {
    memcpy(buffer, field->data.initial, field->data.capacity);
    }
  buffer[field->data.capacity] = '\0';

  // We have the original nul-terminated text at data.initial.  We have a
  // copy of it in buffer[] in the internal codeset.

  // We will reuse a single static structure for each string
  static std::unordered_map<std::string, int> seen_before;
  std::string field_string(buffer);
  std::unordered_map<std::string, int>::const_iterator it =
              seen_before.find(field_string);

  static const char name_base[] = "_literal_a_";

  if( it != seen_before.end() )
    {
    // We've seen that string before.
    int nvar = it->second;
    char ach[32];
    sprintf(ach, "%s%d", name_base, nvar);
    field->var_decl_node = gg_declare_variable(cblc_field_type_node,
                                                  ach,
                                                  NULL,
                                                  vs_file_static);
    }
  else
    {
    // We have not seen that string before
    static int nvar = 1;
    seen_before[field_string] = nvar;

    char ach[32];
    sprintf(ach, "%s%d", name_base, nvar);
    field->var_decl_node  = gg_define_variable( cblc_field_type_node,
                                                ach,
                                                vs_file_static);
    actually_create_the_static_field(
                field,
                build_string_literal(field->data.capacity+1,
                                     buffer),
                field->data.capacity+1,
                field->data.initial,
                NULL_TREE,
                field->var_decl_node);
    TREE_READONLY(field->var_decl_node) = 1;
    TREE_USED(field->var_decl_node) = 1;
    TREE_STATIC(field->var_decl_node) = 1;
    DECL_PRESERVE_P (field->var_decl_node) = 1;
    nvar += 1;
    }
  TRACE1
    {
    TRACE1_INDENT
    TRACE1_TEXT("Finished")
    TRACE1_END
    }
  }
#endif

void
parser_local_add(struct cbl_field_t *new_var )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD(" ", new_var);
    SHOW_PARSE_END
    }

  IF( member(new_var->var_decl_node, "data"),
      ne_op,
      gg_cast(UCHAR_P, null_pointer_node) )
    {
    gg_call(VOID,
            "__gg__push_local_variable",
            gg_get_address_of(new_var->var_decl_node),
          NULL_TREE);
    }
  ELSE
    ENDIF

  if( new_var->level == LEVEL01 || new_var->level == LEVEL77)
    {
    // We need to allocate memory on the stack for this variable
    tree array_type = build_array_type_nelts(UCHAR, new_var->data.capacity);
    tree data_decl_node = gg_define_variable( array_type,
                                                    NULL,
                                                    vs_stack);
    gg_assign( member(new_var->var_decl_node, "data"),
                      gg_get_address_of(data_decl_node) );
    }
  cbl_refer_t wrapper;
  wrapper.field = new_var;
  initialize_variable_internal(wrapper);
  }

void
parser_field_attr_set( cbl_field_t *tgt, cbl_field_attr_t attr, bool on_off )
  {
  if( on_off )
    {
    gg_assign(member(tgt, "attr"),
              gg_bitwise_or(member(tgt, "attr"),
                            build_int_cst_type(SIZE_T, attr)));
    }
  else
    {
    gg_assign(member(tgt, "attr"),
              gg_bitwise_and(member(tgt, "attr"),
                             build_int_cst_type(SIZE_T, ~attr)));
    }
  }

void
parser_symbol_add(struct cbl_field_t *new_var )
  {
  Analyze();
  SHOW_PARSE
    {
    do
      {
      fprintf(stderr, "( %d ) %s():", CURRENT_LINE_NUMBER, __func__);
      }
    while(0);

    fprintf(stderr, " %2.2d %s<%s> off:%zd "
                    "msiz:%d cap:%d dig:%d rdig:%d attr:0x%lx loc:%p",
            new_var->level,
            new_var->name,
            cbl_field_type_str(new_var->type),
            new_var->offset,
            new_var->data.memsize,
            new_var->data.capacity,
            new_var->data.digits,
            new_var->data.rdigits,
            new_var->attr,
            (void*)new_var);

    if( is_table(new_var) )
      {
      fprintf(stderr," OCCURS:%zd", new_var->occurs.ntimes());
      }
    cbl_field_t *parent = parent_of(new_var);
    if( parent )
      {
      fprintf(stderr,
              " parent:(%zd)%s",
              new_var->parent,
              parent->name);
      }
    else
      {
      // Parent isn't a field
      size_t parent_index = new_var->parent;
      if( parent_index )
        {
        symbol_elem_t *e = symbol_at(parent_index);
        if( e->type == SymFile )
          {
          fprintf(stderr,
                  " parent_file:(%zd)%s",
                  new_var->parent,
                  e->elem.file.name);
          if( e->elem.file.attr & external_e )
            {
            fprintf(stderr, " (flagged external)");
            }
          }
        }
      }

    if( symbol_redefines(new_var) )
      {
      fprintf(stderr,
              " redefines:(%p)%s",
              (void*)symbol_redefines(new_var),
              symbol_redefines(new_var)->name);
      }

    SHOW_PARSE_END
    }

  if( new_var->level == 1  && new_var->occurs.bounds.upper )
    {
    if( new_var->data.memsize < new_var->data.capacity * new_var->occurs.bounds.upper )
      {
      cbl_internal_error("LEVEL 01 (%s) OCCURS "
                         "has insufficient data.memsize", new_var->name);
      }
    }

  if( new_var->var_decl_node )
    {
    if( new_var->type != FldConditional )
      {
      // There is a possibility when re-using variables that a temporary that
      // was created at compile time might not have a data pointer at run time.
      if( new_var->attr & (intermediate_e) )
        {
        IF( member(new_var->var_decl_node, "allocated"),
            lt_op,
            member(new_var->var_decl_node, "capacity") )
          {
          gg_free(member(new_var, "data"));
          gg_assign(member(new_var, "data"),
                    gg_cast(UCHAR_P, gg_malloc(new_var->data.capacity)));
          gg_assign(member(new_var, "allocated"),
                    build_int_cst_type(SIZE_T, new_var->data.capacity));
          }
        ELSE
          {
          }
        ENDIF
        }
      }
    else
      {
      gg_assign(new_var->var_decl_node, boolean_false_node);
      }

    goto done;
    }

  if( !(new_var->attr & initialized_e) )
    {
    cbl_field_type_t incoming_type = new_var->type;

    if( is_register_field(new_var) )
      {
      psa_global(new_var);
      goto done;
      }

    if( new_var->type == FldBlob )
      {
      psa_FldBlob(new_var);
      goto done;
      }

    if( new_var->type == FldLiteralA )
      {
      new_var->data.picture = "";
      psa_FldLiteralA(new_var);
      goto done;
      }

    size_t length_of_initial_string = 0;
    const char *new_initial = NULL;

    // gg_printf("parser_symbol_add %s\n", build_string_literal( strlen(new_var->name)+1, new_var->name), NULL_TREE);

    // If we are dealing with an alphanumeric, and it is not hex_encoded, we
    // want to convert to single-byte-encoding (if it happens to be UTF-8) and
    // to EBCDIC, if EBCDIC is in force:

    //  Make sure we have a new variable to work with.
    if( !new_var )
      {
      cbl_internal_error("parser_symbol_add() was called with a NULL new_var\n");
      }

    TRACE1
      {
      TRACE1_HEADER
      if( new_var->level )
        {
        gg_fprintf( trace_handle,
                    1,
                    "%2.2d ",
                    build_int_cst_type(INT, new_var->level));
        }
      TRACE1_TEXT(new_var->name)
      TRACE1_TEXT_ABC(" (", cbl_field_type_str(new_var->type) ,")")
      if( new_var->type == FldLiteralN)
        {
        gg_fprintf( trace_handle,
                    1, " [%ld]",
                    build_int_cst_type(LONG,
                                        *(const long *)new_var->data.initial));
        }
      TRACE1_END
      }

    if( is_table(new_var) && new_var->data.capacity == 0)
      {
      cbl_internal_error(
          "%s(): %2.2d %s is a table, but it improperly has a capacity of zero",
           __func__,
           new_var->level,
           new_var->name);
      }

    cbl_field_t *ancestor = NULL;
    tree immediate_parent = NULL_TREE;

    if( new_var->parent > 0 )
      {
      symbol_elem_t *parent = symbol_at(new_var->parent);
      gcc_assert(parent);
      if( parent->type == SymField )
        {
        ancestor = cbl_field_of(parent);
        immediate_parent = ancestor->var_decl_node;
        }
      }

    if( ancestor == NULL )
      {
      // This is a last ditch effort for handling SAME AREA.  Although
      // symbol_redefines should work for REDEFINES, LEVEL66, and SAME AREA, I
      // decided to leave the existing code alone and added this in when SAME AREA
      // was added in.
      ancestor = symbol_redefines(new_var);
      if( ancestor )
        {
        immediate_parent = ancestor->var_decl_node;

        // This obscure test was put in to find problems caused by SAME AREA,
        // which at one point would cause a parent to be erroneously seen after
        // the child.
        assert(ancestor->our_index < new_var->our_index);
        }
      }

    if( ancestor == new_var )
      {
      cbl_internal_error("parser_symbol_add(): %s is its own ancestor",
                          new_var->name);
      }

    if( !ancestor && (new_var->level > LEVEL01 && new_var->level <= LEVEL49 ) )
      {
      cbl_internal_error("parser_symbol_add(): %2.2d %s has null ancestor",
            new_var->level,
            new_var->name);
      }

    //  new_var's var_decl_node should be NULL at this point
    if( new_var->var_decl_node )
      {
      cbl_internal_error( "parser_symbol_add( %s ) improperly has a non-null "
             "var_decl_node\n",
             new_var->name);
      }

    switch( new_var->type )
      {
      static int counter=1;
      char ach[2*sizeof(cbl_name_t)];
      case FldConditional:
        // FldConditional corresponds to a C "bool".  But we don't carry
        // a runtime copy of a structure for the variable; instead,
        // var_decl_node becomes a boolean_type_node that is used directly.
        sprintf(ach, "_%sconditional_%d", new_var->name, counter++);
        new_var->var_decl_node = gg_define_variable(BOOL, ach, vs_static);
        goto done;
        break;

      default:
        break;
      }

    if(    new_var->type == FldNumericBinary
        || new_var->type == FldNumericBin5 )
      {
      switch( new_var->data.capacity )
        {
        case 1:
        case 2:
        case 4:
        case 8:
        case 16:
          break;
        default:
          fprintf(stderr,
                  "%s is type %s and has capacity %u\n",
                  new_var->name,
                  cbl_field_type_str(new_var->type),
                  new_var->data.capacity);
          gcc_unreachable();
          break;
        }
      }

    size_t level_88_string_size = 0;
    char *level_88_string = NULL;
    if( ancestor )
      {
      level_88_string = get_level_88_domain(ancestor->data.capacity, new_var, level_88_string_size);
      }

    if( !new_var->data.picture )
      {
      // When picture is NULL, we have to keep testing for NULLness at runtime
      // Force it to be a zero-length string here, so that we don't need to
      // worry about it.
      new_var->data.picture = "";
      }

    if( new_var->type == FldNumericEdited && (new_var->attr & scaled_e) )
      {
      char *pic = xstrdup(new_var->data.picture); // duplicate the const char *
      remove_p_from_picture(pic);
      new_var->data.picture = pic;
      }

    if( new_var->type == FldClass && new_var->level != 88 )
      {
      new_var->data.initial = get_class_condition_string(new_var);
      }

    if( new_var->type == FldLiteralA )
      {
      length_of_initial_string = new_var->data.capacity;
      }
    else if( new_var->data.initial && new_var->data.initial[0] != '\0' )
      {
      if( new_var->type == FldClass )
        {
        length_of_initial_string = strlen(new_var->data.initial)+1;
        }
      else if( new_var->type == FldNumericDisplay )
        {
        length_of_initial_string = strlen(new_var->data.initial)+1;
        }
      else
        {
        // This is an ordinary string
        // fprintf(stderr, ">>>>>>> parser_symbol_add %s %s \n", cbl_field_type_str(new_var->type), new_var->name);
        // fprintf(stderr, "        %d %d\n", (int)strlen(new_var->data.initial), (int)new_var->data.capacity);
        //length_of_initial_string = strlen(new_var->data.initial) + 1;
        length_of_initial_string = new_var->data.capacity + 1;
        }
      }
    else
      {
      // We have something that doesn't have a data.initial pointer
      length_of_initial_string = 0;
      }

    // GDB needs to know the data hierarchy.  We do that by including our_index
    // and parent index in the variable name:

    size_t our_index = new_var->our_index;

    // During the early stages of implementing cbl_field_t::our_index, there
    // were execution paths in parse.y and parser.cc that resulted in our_index
    // not being set.  I hereby try to use field_index() to find the index
    // of this field to resolve those.  I note that field_index does a linear
    // search of the symbols[] table to find that index.  That's why I don't
    // use it routinely; it results in O(N^squared) computational complexity
    // to do a linear search of the symbol table for each symbol

    if(   !our_index
          && new_var->type != FldLiteralN
          && !(new_var->attr & intermediate_e))
      {
      our_index = field_index(new_var);
      if( our_index == (size_t)-1 )
        {
        // Hmm.  Couldn't find it.  Seems odd.
        our_index = 0;
        }
      }

    // When we create the cblc_field_t structure, we need a data pointer
    // for "data".  In the case of a variable that has no parent, we
    // have to allocate storage.  In the case of a variable that has a parent,
    // we calculate data as the pointer to our parent's data plus our
    // offset.

    // declare and define the structure.  This code *must* match
    // the C structure declared in libgcobol.c.  Towards that end, the
    // variables are declared in descending order of size in order to
    // make the packing match up.

    // This uses a single structure type_decl template for creating each structure

    char external_record_base[2*sizeof(cbl_name_t)] = "";

    if( new_var->parent > 0 )
      {
      symbol_elem_t *parent = symbol_at(new_var->parent);
      gcc_assert(parent);
      if( parent->type == SymField )
        {
        ancestor = cbl_field_of(parent);
        immediate_parent = ancestor->var_decl_node;
        }
      else if( parent->type == SymFile )
        {
        if( parent->elem.file.attr & external_e )
          {
          // The parent of new_var is a SymFile with the external_e attribute
          // Therefore, we have to establish new_var as an external with a
          // predictable name
          strcpy(external_record_base, parent->elem.file.name);
          }
        }
      }

    tree new_var_decl = psa_new_var_decl(new_var, external_record_base);

    if( new_var->type == FldNumericEdited )
      {
      // Decide if a NumericEdited can hold negative numbers:
      size_t len = strlen( new_var->data.picture);

      new_var->attr &= ~signable_e;
      if( strchr(new_var->data.picture, '+') )
        {
        new_var->attr |= signable_e;
        }
      else if( strchr(new_var->data.picture, '-') )
        {
        new_var->attr |= signable_e;
        }
      else if( len > 2 )
        {
        char ch1 = _toupper(new_var->data.picture[len-2]);
        char ch2 = _toupper(new_var->data.picture[len-1]);
        if(    (ch1 == 'D' && ch2 == 'B')
               || (ch1 == 'C' && ch2 == 'R') )
          {
          new_var->attr |= signable_e;
          }
        }
      }

    /*
     * Burn after reading. (Delete comment after implementing.)
     *
     * As of Tue Apr  4 10:29:35 2023, we support 01 CONSTANT numeric values as follows:
     * 1.  FldNumericBin5
     * 2.  always constant_e, also potentially global_e
     * 3.  compile-time value in cbl_field_data_t::value
     * 4.  cbl_field_data_t::capacity is 0 because it requires no working storage
     */

    if( new_var->data.capacity == 0
        && new_var->level != 88
        && new_var->type  != FldClass
        && new_var->type  != FldLiteralN
        && new_var->type  != FldLiteralA )
      {
      cbl_internal_error(  "%s(): %2.2d %s<%s> improperly has a data.capacity of zero",
              __func__,
              new_var->level,
              new_var->name,
              cbl_field_type_str(new_var->type));
      }

    new_var->var_decl_node = new_var_decl;

    if( level_88_string )
      {
      new_var->data.initial = level_88_string;
      length_of_initial_string = level_88_string_size;
      }

    tree data_area = null_pointer_node;

    if( *external_record_base )
      {
      char achDataName[256];
      if( *external_record_base )
        {
        sprintf(achDataName, "__%s_vardata", external_record_base);
        }
      tree array_type = build_array_type_nelts(UCHAR, new_var->data.capacity);
      new_var->data_decl_node = gg_define_variable(
                          array_type,
                          achDataName,
                          vs_external);
      data_area = gg_get_address_of(new_var->data_decl_node);
      goto actual_allocate;
      }

    if( ancestor && new_var->level != 0 )
      {
      // This variable has an ancestor, so we share its already-allocated data
      // area
      new_var->data_decl_node = ancestor->data_decl_node;
      }
    else
      {
      // We have no ancestor, so data_decl_node must be allocated.  Note that
      // LEVEL00 variables might have ancestors (INDEXED BY variables, for
      // example), but they need data allocated.

      if( new_var->type == FldLiteralN )
        {
        // A numeric literal gets special handling:
        psa_FldLiteralN(new_var);
        data_area = gg_get_address_of(new_var->data_decl_node);
        }
      else
        {
        // Create a static array of UCHAR, and make that the data_decl_node
        // size_t bytes_to_allocate = new_var->data.memsize ?
                                // new_var->data.memsize : new_var->data.capacity;
        size_t bytes_to_allocate = std::max(new_var->data.memsize,
                                            new_var->data.capacity);

        // A FldClass actually doesn't need any bytes, because the only important
        // thing about it is the .initial field.  We will allocate a single byte,
        // just to keep run-time pointers from being NULL
        if(    (new_var->type == FldClass    && bytes_to_allocate == 0)
            || (new_var->type == FldLiteralA && bytes_to_allocate == 0)  )
          {
          bytes_to_allocate = 1;
          }

        if( !bytes_to_allocate )
          {
          fprintf(stderr,
                  "bytes_to_allocate is zero for %s (symbol number %ld)\n",
                  new_var->name,
                  new_var->our_index);
          gcc_assert(bytes_to_allocate);
          }

        if( new_var->type == FldIndex && new_var->level == 0 )
          {
          // Do nothing, because the OCCURS INDEXED BY variable needs data
          // allocated.  This leaves bytes_to_allcate at its value.
          }
        else
          {
          if(    new_var->attr & based_e
              || new_var->attr & linkage_e
              || new_var->attr & local_e )
            {
            // BASED   variables get their data through ALLOCATE or SET
            // LINKAGE variables get their data from the caller
            // LOCAL   variables get their data dynamically.
            bytes_to_allocate = 0;
            }
          }

        if( bytes_to_allocate )
          {
          // We need a unique name for the allocated data for this COBOL variable:
          char achDataName[256];
          if( new_var->attr & external_e )
            {
            sprintf(achDataName, "%s", new_var->name);
            }
          else if( new_var->name[0] == '_' )
            {
            // Avoid doubling up on leading underscore
            sprintf(achDataName,
                    "%s_data_%lu",
                    new_var->name,
                    sv_data_name_counter++);
            }
          else
            {
            sprintf(achDataName,
                    "_%s_data_%lu",
                    new_var->name,
                    sv_data_name_counter++);
            }

          if( new_var->attr & external_e )
            {
            tree array_type = build_array_type_nelts(UCHAR, bytes_to_allocate);
            new_var->data_decl_node = gg_define_variable(
                                array_type,
                                achDataName,
                                vs_external);
            data_area = gg_get_address_of(new_var->data_decl_node);
            }
          else
            {
            gg_variable_scope_t vs_scope = (new_var->attr & intermediate_e)
                                            ? vs_stack : vs_static ;
            tree array_type = build_array_type_nelts(UCHAR, bytes_to_allocate);
            new_var->data_decl_node = gg_define_variable(
                                array_type,
                                achDataName,
                                vs_scope);
            data_area = gg_get_address_of(new_var->data_decl_node);
            }
          }
        }
      }

    if( new_var->data.initial )
      {
      new_initial = initial_from_float128(new_var);
      }
    if( new_initial )
      {
      switch(new_var->type)
        {
        case FldGroup:
        case FldAlphanumeric:
        case FldLiteralA:
          length_of_initial_string = new_var->data.capacity+1;
          break;

        default:
          length_of_initial_string = new_var->data.capacity;
          break;
        }
      }
    else
      {
      new_initial = new_var->data.initial;
      if( !new_initial )
        {
        if( length_of_initial_string )
          {
          gcc_unreachable();
          }
        }
      else
        {
        if( new_var->type == FldLiteralN )
          {
          // We need to convert this string to the internal character set
          // char *buffer = NULL;
          // size_t buffer_size = 0;
          // raw_to_internal(&buffer,
                          // &buffer_size,
                          // new_var->data.initial,
                          // strlen(new_var->data.initial));
          // new_initial = bufer;
          // length_of_initial_string = strlen(new_var->data.initial)+1;
          }
        }
      }

    actual_allocate:
    // if( level_88_string )
      // {
      // actually_create_the_static_field( new_var,
                                        // data_area,
                                        // level_88_string_size,
                                        // level_88_string,
                                        // immediate_parent,
                                        // new_var_decl);
      // }
    // else
      {
      actually_create_the_static_field( new_var,
                                        data_area,
                                        length_of_initial_string,
                                        new_initial,
                                        immediate_parent,
                                        new_var_decl);
      }

    if( level_88_string )
      {
      free(level_88_string);
      }

    if(  !(new_var->attr & ( linkage_e | based_e)) )
      {
      static const bool explicitly = false;
      static const bool just_once = true;
      initialize_variable_internal( new_var,
                                    explicitly,
                                    just_once);
      }

    if( new_var->type != incoming_type )
      {
      fprintf(stderr, "Type mismatch in parser_symbol_add()\n");
      gcc_unreachable();
      }
    new_var->attr |= initialized_e;
    }
  else
    {
    fprintf(stderr, "parser_symbol_add() skipping %s", new_var->name);
    }
  done:
  return;
  }
