/* C code produced by gperf version 2.7.1 (19981006 egcs) */
/* Command-line: gperf -L C -F , 0, 0, 0 -D -E -S1 -p -j1 -i 1 -g -o -t -k* gperf.tmp  */
struct resword {
  const char   *name;
  short        token;
  enum rid     rid;
  enum toktype { RESERVED, DIRECTIVE, PREDEF } flags;
};
extern tree ridpointers [];
/* maximum key range = 2815, duplicates = 6 */

#ifdef __GNUC__
__inline
#endif
static unsigned int
hash (str, len)
     register const char *str;
     register unsigned int len;
{
  static unsigned short asso_values[] =
    {
      2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822,
      2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822,
      2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822,
      2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822,
      2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822,
      2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822,
      2822, 2822, 2822, 2822, 2822,    4,   61,   80,   12,  350,
        91,   39,    3,    2, 2822,    4,  129,  155,   64,   46,
        65, 2822,   96,   13,    1,  135,    7,    2,    8,  124,
         7, 2822, 2822, 2822, 2822,    1, 2822,   94,   40,  127,
        21,    1,   81,    1,    1,    7, 2822,    3,   23,   74,
       255,  203,   70, 2822,  218,    1,   88,  124,    1,    6,
        10,   56,   40, 2822, 2822, 2822, 2822, 2822, 2822, 2822,
      2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822,
      2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822,
      2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822,
      2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822,
      2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822,
      2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822,
      2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822,
      2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822,
      2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822,
      2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822,
      2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822,
      2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822, 2822,
      2822, 2822, 2822, 2822, 2822, 2822
    };
  register int hval = len;

  switch (hval)
    {
      default:
      case 30:
        hval += asso_values[(unsigned char)str[29]];
      case 29:
        hval += asso_values[(unsigned char)str[28]];
      case 28:
        hval += asso_values[(unsigned char)str[27]];
      case 27:
        hval += asso_values[(unsigned char)str[26]];
      case 26:
        hval += asso_values[(unsigned char)str[25]];
      case 25:
        hval += asso_values[(unsigned char)str[24]];
      case 24:
        hval += asso_values[(unsigned char)str[23]];
      case 23:
        hval += asso_values[(unsigned char)str[22]];
      case 22:
        hval += asso_values[(unsigned char)str[21]];
      case 21:
        hval += asso_values[(unsigned char)str[20]];
      case 20:
        hval += asso_values[(unsigned char)str[19]];
      case 19:
        hval += asso_values[(unsigned char)str[18]];
      case 18:
        hval += asso_values[(unsigned char)str[17]];
      case 17:
        hval += asso_values[(unsigned char)str[16]];
      case 16:
        hval += asso_values[(unsigned char)str[15]];
      case 15:
        hval += asso_values[(unsigned char)str[14]];
      case 14:
        hval += asso_values[(unsigned char)str[13]];
      case 13:
        hval += asso_values[(unsigned char)str[12]];
      case 12:
        hval += asso_values[(unsigned char)str[11]];
      case 11:
        hval += asso_values[(unsigned char)str[10]];
      case 10:
        hval += asso_values[(unsigned char)str[9]];
      case 9:
        hval += asso_values[(unsigned char)str[8]];
      case 8:
        hval += asso_values[(unsigned char)str[7]];
      case 7:
        hval += asso_values[(unsigned char)str[6]];
      case 6:
        hval += asso_values[(unsigned char)str[5]];
      case 5:
        hval += asso_values[(unsigned char)str[4]];
      case 4:
        hval += asso_values[(unsigned char)str[3]];
      case 3:
        hval += asso_values[(unsigned char)str[2]];
      case 2:
        hval += asso_values[(unsigned char)str[1]];
      case 1:
        hval += asso_values[(unsigned char)str[0]];
        break;
    }
  return hval;
}

#ifdef __GNUC__
__inline
#endif
struct resword *
in_word_set (str, len)
     register const char *str;
     register unsigned int len;
{
  enum
    {
      TOTAL_KEYWORDS = 300,
      MIN_WORD_LENGTH = 2,
      MAX_WORD_LENGTH = 30,
      MIN_HASH_VALUE = 7,
      MAX_HASH_VALUE = 2821
    };

  static struct resword wordlist[] =
    {
      {"AT",			AT,			NORID,	RESERVED},
      {"WITH",			WITH,			NORID,	RESERVED},
      {"THIS",			THIS,			NORID,	RESERVED},
      {"else",			ELSE,			NORID,	RESERVED},
      {"while",			WHILE,			NORID,	RESERVED},
      {"TO",			TO,			NORID,	RESERVED},
      {"seize",			SEIZE,			NORID,	RESERVED},
      {"DO",			DO,			NORID,	RESERVED},
      {"OD",			OD,			NORID,	RESERVED},
      {"BIT",			BOOLS,			RID_BOOLS,	PREDEF},
      {"IN",			IN,			RID_IN,	RESERVED},
      {"INIT",			INIT,			NORID,	RESERVED},
      {"AND",			AND,			NORID,	RESERVED},
      {"fi",			FI,			NORID,	RESERVED},
      {"if",			IF,			NORID,	RESERVED},
      {"set",			SET,			NORID,	RESERVED},
      {"FI",			FI,			NORID,	RESERVED},
      {"IF",			IF,			NORID,	RESERVED},
      {"by",			BY,			NORID,	RESERVED},
      {"this",			THIS,			NORID,	RESERVED},
      {"with",			WITH,			NORID,	RESERVED},
      {"STATIC",			STATIC,			NORID,	RESERVED},
      {"exit",			EXIT,			NORID,	RESERVED},
      {"ON",			ON,			NORID,	RESERVED},
      {"NOT",			NOT,			NORID,	RESERVED},
      {"elsif",			ELSIF,			NORID,	RESERVED},
      {"START",			START,			NORID,	RESERVED},
      {"list",			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"POS",			POS,			NORID,	RESERVED},
      {"DOWN",			DOWN,			NORID,	RESERVED},
      {"STOP",			STOP,			NORID,	RESERVED},
      {"BIN",			BIN,			NORID,	RESERVED},
      {"GOTO",			GOTO,			NORID,	RESERVED},
      {"bit",			BOOLS,			RID_BOOLS,	PREDEF},
      {"OF",			OF,			NORID,	RESERVED},
      {"all",			ALL,			NORID,	RESERVED},
      {"OR",			OR,			NORID,	RESERVED},
      {"ROW",			ROW,			NORID,	RESERVED},
      {"LIST",			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"XOR",			XOR,			NORID,	RESERVED},
      {"PACK",			PACK,			NORID,	RESERVED},
      {"based",			BASED,			NORID,	RESERVED},
      {"step",			STEP,			NORID,	RESERVED},
      {"page",			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"asm",			ASM_KEYWORD,		NORID,	RESERVED},
      {"dcl",			DCL,			NORID,	RESERVED},
      {"ASM",			ASM_KEYWORD,		NORID,	RESERVED},
      {"ANDIF",			ANDIF,			NORID,	RESERVED},
      {"simple",			SIMPLE,			NORID,	RESERVED},
      {"at",			AT,			NORID,	RESERVED},
      {"OUT",			PARAMATTR,		RID_OUT,	RESERVED},
      {"BY",			BY,			NORID,	RESERVED},
      {"text",			TEXT,			NORID,	RESERVED},
      {"FAR",			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"up",			UP,			NORID,	RESERVED},
      {"delay",			DELAY,			NORID,	RESERVED},
      {"CHARS",			CHARS,			NORID,	RESERVED},
      {"UP",			UP,			NORID,	RESERVED},
      {"spec",			SPEC,			NORID,	RESERVED},
      {"SYN",			SYN,			NORID,	RESERVED},
      {"GRANT",			GRANT,			NORID,	RESERVED},
      {"MOD",			MOD,			NORID,	RESERVED},
      {"small",			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"DCL",			DCL,			NORID,	RESERVED},
      {"ever",			EVER,			NORID,	RESERVED},
      {"do",			DO,			NORID,	RESERVED},
      {"od",			OD,			NORID,	RESERVED},
      {"case",			CASE,			NORID,	RESERVED},
      {"esac",			ESAC,			NORID,	RESERVED},
      {"CCITT_OS",		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"FOR",			FOR,			NORID,	RESERVED},
      {"ORIF",			ORIF,			NORID,	RESERVED},
      {"BODY",			BODY,			NORID,	RESERVED},
      {"INOUT",			PARAMATTR,		RID_INOUT,	RESERVED},
      {"SIGNAL",			SIGNAL,			NORID,	RESERVED},
      {"LOC",			LOC,			NORID,	RESERVED},
      {"NOLIST",			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"even",			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"in",			IN,			RID_IN,	RESERVED},
      {"ALL",			ALL,			NORID,	RESERVED},
      {"NOPACK",			NOPACK,			NORID,	RESERVED},
      {"call",			CALL,			NORID,	RESERVED},
      {"pos",			POS,			NORID,	RESERVED},
      {"end",			END,			NORID,	RESERVED},
      {"send",			SEND,			NORID,	RESERVED},
      {"of",			OF,			NORID,	RESERVED},
      {"PROC",			PROC,			NORID,	RESERVED},
      {"to",			TO,			NORID,	RESERVED},
      {"rem",			REM,			NORID,	RESERVED},
      {"pack",			PACK,			NORID,	RESERVED},
      {"BOOLS",			BOOLS,			RID_BOOLS,	RESERVED},
      {"mod",			MOD,			NORID,	RESERVED},
      {"ref",			REF,			NORID,	RESERVED},
      {"use_seize_file",		USE_SEIZE_FILE,		NORID,	DIRECTIVE},
      {"bin",			BIN,			NORID,	RESERVED},
      {"medium",			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"begin",			BEGINTOKEN,		NORID,	RESERVED},
      {"FORBID",			FORBID,			NORID,	RESERVED},
      {"syn",			SYN,			NORID,	RESERVED},
      {"body",			BODY,			NORID,	RESERVED},
      {"ARRAY",			ARRAY,			NORID,	RESERVED},
      {"STRUCT",			STRUCT,			NORID,	RESERVED},
      {"read",			READ,			RID_READ,	RESERVED},
      {"cycle",			CYCLE,			NORID,	RESERVED},
      {"large",			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"VARYING",		VARYING,		NORID,	RESERVED},
      {"CALL",			CALL,			NORID,	RESERVED},
      {"then",			THEN,			NORID,	RESERVED},
      {"event",			EVENT,			NORID,	RESERVED},
      {"cause",			CAUSE,			NORID,	RESERVED},
      {"loc",			LOC,			NORID,	RESERVED},
      {"access",			ACCESS,			NORID,	RESERVED},
      {"init",			INIT,			NORID,	RESERVED},
      {"receive",		RECEIVE,		NORID,	RESERVED},
      {"TEXT",			TEXT,			NORID,	RESERVED},
      {"EXIT",			EXIT,			NORID,	RESERVED},
      {"stop",			STOP,			NORID,	RESERVED},
      {"SET",			SET,			NORID,	RESERVED},
      {"and",			AND,			NORID,	RESERVED},
      {"signal",			SIGNAL,			NORID,	RESERVED},
      {"far",			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"assert",			ASSERT,			NORID,	RESERVED},
      {"static",			STATIC,			NORID,	RESERVED},
      {"debug_types",		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"prefixed",		PREFIXED,		NORID,	RESERVED},
      {"out",			PARAMATTR,		RID_OUT,	RESERVED},
      {"THEN",			THEN,			NORID,	RESERVED},
      {"or",			OR,			NORID,	RESERVED},
      {"END",			END,			NORID,	RESERVED},
      {"row",			ROW,			NORID,	RESERVED},
      {"STEP",			STEP,			NORID,	RESERVED},
      {"xor",			XOR,			NORID,	RESERVED},
      {"SMALL",			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"PRIORITY",		PRIORITY,		NORID,	RESERVED},
      {"SEND",			SEND,			NORID,	RESERVED},
      {"BASED",			BASED,			NORID,	RESERVED},
      {"chars",			CHARS,			NORID,	RESERVED},
      {"DYNAMIC",		DYNAMIC,		RID_DYNAMIC,	RESERVED},
      {"CASE",			CASE,			NORID,	RESERVED},
      {"ESAC",			ESAC,			NORID,	RESERVED},
      {"module",			MODULE,			NORID,	RESERVED},
      {"on",			ON,			NORID,	RESERVED},
      {"result",			RESULT,			NORID,	RESERVED},
      {"PAGE",			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"andif",			ANDIF,			NORID,	RESERVED},
      {"READ",			READ,			RID_READ,	RESERVED},
      {"bools",			BOOLS,			RID_BOOLS,	RESERVED},
      {"ASSERT",			ASSERT,			NORID,	RESERVED},
      {"debug_lines",		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"after",			AFTER,			NORID,	RESERVED},
      {"ALL_STATIC_ON",		ALL_STATIC_ON,		NORID,	DIRECTIVE},
      {"down",			DOWN,			NORID,	RESERVED},
      {"WHILE",			WHILE,			NORID,	RESERVED},
      {"start",			START,			NORID,	RESERVED},
      {"optimize",		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"goto",			GOTO,			NORID,	RESERVED},
      {"for",			FOR,			NORID,	RESERVED},
      {"SPEC",			SPEC,			NORID,	RESERVED},
      {"orif",			ORIF,			NORID,	RESERVED},
      {"BEGIN",			BEGINTOKEN,		NORID,	RESERVED},
      {"REF",			REF,			NORID,	RESERVED},
      {"OPTIMIZATION_WINDOW",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"ACCESS",			ACCESS,			NORID,	RESERVED},
      {"AFTER",			AFTER,			NORID,	RESERVED},
      {"not",			NOT,			NORID,	RESERVED},
      {"buffer",			BUFFER,			NORID,	RESERVED},
      {"inline",			INLINE,			RID_INLINE,	RESERVED},
      {"CONTEXT",		CONTEXT,		NORID,	RESERVED},
      {"RANGE",			RANGE,			NORID,	RESERVED},
      {"newmode",		NEWMODE,		NORID,	RESERVED},
      {"range",			RANGE,			NORID,	RESERVED},
      {"forbid",			FORBID,			NORID,	RESERVED},
      {"nolist",			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"CAUSE",			CAUSE,			NORID,	RESERVED},
      {"ELSIF",			ELSIF,			NORID,	RESERVED},
      {"remote",			REMOTE,			NORID,	RESERVED},
      {"timeout",		TIMEOUT,		NORID,	RESERVED},
      {"powerset",		POWERSET,		NORID,	RESERVED},
      {"debug_symbols",		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"general",		GENERAL,		NORID,	RESERVED},
      {"REGION",			REGION,			NORID,	RESERVED},
      {"REM",			REM,			NORID,	RESERVED},
      {"ALL_STATIC_OFF",		ALL_STATIC_OFF,		NORID,  DIRECTIVE},
      {"INLINE",			INLINE,			RID_INLINE,	RESERVED},
      {"synmode",		SYNMODE,		NORID,	RESERVED},
      {"proc",			PROC,			NORID,	RESERVED},
      {"LARGE",			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"DELAY",			DELAY,			NORID,	RESERVED},
      {"process",		PROCESS,		NORID,	RESERVED},
      {"OPTIMIZE",		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"varying",		VARYING,		NORID,	RESERVED},
      {"dynamic",		DYNAMIC,		RID_DYNAMIC,	RESERVED},
      {"ccitt_os",		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"struct",			STRUCT,			NORID,	RESERVED},
      {"grant",			GRANT,			NORID,	RESERVED},
      {"empty_off",		EMPTY_OFF,		NORID,	DIRECTIVE},
      {"PROCESS",		PROCESS,		NORID,	RESERVED},
      {"RANGE_ON",		RANGE_ON,		NORID,	DIRECTIVE},
      {"inout",			PARAMATTR,		RID_INOUT,	RESERVED},
      {"array",			ARRAY,			NORID,	RESERVED},
      {"region",			REGION,			NORID,	RESERVED},
      {"TIMEOUT",		TIMEOUT,		NORID,	RESERVED},
      {"recursive",		RECURSIVE,		NORID,	RESERVED},
      {"event_code",		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"NONREF",			NONREF,			NORID,	RESERVED},
      {"SIMPLE",			SIMPLE,			NORID,	RESERVED},
      {"SEIZE",			SEIZE,			NORID,	RESERVED},
      {"RESULT",			RESULT,			NORID,	RESERVED},
      {"multiple_data_segs",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"signal_code",		SIGNAL_CODE,		NORID,	DIRECTIVE},
      {"RETURN",			RETURN,			NORID,	RESERVED},
      {"CONTINUE",		CONTINUE,		NORID,	RESERVED},
      {"SIGNAL_CODE",		SIGNAL_CODE,		NORID,	DIRECTIVE},
      {"empty_on",		EMPTY_ON,		NORID,	DIRECTIVE},
      {"nopack",			NOPACK,			NORID,	RESERVED},
      {"RETURNS",		RETURNS,		NORID,	RESERVED},
      {"CYCLE",			CYCLE,			NORID,	RESERVED},
      {"SYNMODE",		SYNMODE,		NORID,	RESERVED},
      {"exceptions",		EXCEPTIONS,		NORID,	RESERVED},
      {"EVEN",			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"PRINT_O_CODE",		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"EVENT",			EVENT,			NORID,	RESERVED},
      {"context",		CONTEXT,		NORID,	RESERVED},
      {"RANGE_OFF",		RANGE_OFF,		NORID,	DIRECTIVE},
      {"EVER",			EVER,			NORID,	RESERVED},
      {"EMPTY_ON",		EMPTY_ON,		NORID,	DIRECTIVE},
      {"MEDIUM",			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"BUFFER",			BUFFER,			NORID,	RESERVED},
      {"MODULE",			MODULE,			NORID,	RESERVED},
      {"grant_file_size",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"ELSE",			ELSE,			NORID,	RESERVED},
      {"process_type",		PROCESS_TYPE_TOKEN,	NORID,	DIRECTIVE},
      {"priority",		PRIORITY,		NORID,	RESERVED},
      {"buffer_code",		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"return",			RETURN,			NORID,	RESERVED},
      {"returns",		RETURNS,		NORID,	RESERVED},
      {"all_static_off",		ALL_STATIC_OFF,		NORID,  DIRECTIVE},
      {"POWERSET",		POWERSET,		NORID,	RESERVED},
      {"EMPTY_OFF",		EMPTY_OFF,		NORID,	DIRECTIVE},
      {"range_off",		RANGE_OFF,		NORID,	DIRECTIVE},
      {"signal_max_length",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"PREFIXED",		PREFIXED,		NORID,	RESERVED},
      {"NEWMODE",		NEWMODE,		NORID,	RESERVED},
      {"EXCEPTIONS",		EXCEPTIONS,		NORID,	RESERVED},
      {"REMOTE",			REMOTE,			NORID,	RESERVED},
      {"SHORT_PRED_SUCC",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"all_static_on",		ALL_STATIC_ON,		NORID,	DIRECTIVE},
      {"nonref",			NONREF,			NORID,	RESERVED},
      {"SIGNAL_MAX_LENGTH",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"REENTRANT",		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"range_on",		RANGE_ON,		NORID,	DIRECTIVE},
      {"GENERAL",		GENERAL,		NORID,	RESERVED},
      {"continue",		CONTINUE,		NORID,	RESERVED},
      {"STATE_ROUTINE",		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"EXTRA_CONST_SEG",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"use_seize_file_restricted",	USE_SEIZE_FILE_RESTRICTED,	NORID,	DIRECTIVE},
      {"ONLY_FOR_TARGET",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"extra_const_seg",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"multiple_const_segs",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"RECURSIVE",		RECURSIVE,		NORID,	RESERVED},
      {"DEBUG_SYMBOLS",		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"DEBUG_TYPES",		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"GRANT_FILE_SIZE",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"DEBUG_LINES",		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"ONLY_FOR_SIMULATION",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"state_routine",		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"generate_set_names",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"print_o_code",		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"PROCESS_TYPE",		PROCESS_TYPE_TOKEN,	NORID,	DIRECTIVE},
      {"short_pred_succ",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"reentrant",		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"RECEIVE",		RECEIVE,		NORID,	RESERVED},
      {"EVENT_CODE",		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"optimize_runtime",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"SUPPORT_CAUSING_ADDRESS",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"print_symbol_table",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"REENTRANT_ALL",		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"PRINT_SYMBOL_TABLE",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"BUFFER_CODE",		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"generate_all_set_names",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"NO_OVERLAP_CHECK",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"no_overlap_check",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"reentrant_all",		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"MULTIPLE_DATA_SEGS",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"OPTIMIZE_RUNTIME",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"only_for_target",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"MULTIPLE_CONST_SEGS",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"optimization_window",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"support_causing_address",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"USE_SEIZE_FILE",		USE_SEIZE_FILE,		NORID,	DIRECTIVE},
      {"SEND_SIGNAL_DEFAULT_PRIORITY",	SEND_SIGNAL_DEFAULT_PRIORITY,	NORID,	DIRECTIVE},
      {"make_publics_for_discrete_syns",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"only_for_simulation",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"send_signal_default_priority",	SEND_SIGNAL_DEFAULT_PRIORITY,	NORID,	DIRECTIVE},
      {"send_buffer_default_priority",	SEND_BUFFER_DEFAULT_PRIORITY,	NORID,	DIRECTIVE},
      {"GENERATE_SET_NAMES",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"MAKE_PUBLICS_FOR_DISCRETE_SYNS",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"SEND_BUFFER_DEFAULT_PRIORITY",	SEND_BUFFER_DEFAULT_PRIORITY,	NORID,	DIRECTIVE},
      {"GENERATE_ALL_SET_NAMES",	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"USE_SEIZE_FILE_RESTRICTED",	USE_SEIZE_FILE_RESTRICTED,	NORID,	DIRECTIVE}
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= MIN_HASH_VALUE)
        {
          register struct resword *wordptr;
          register struct resword *wordendptr;
          register struct resword *resword;

          switch (key - 7)
            {
              case 0:
                resword = &wordlist[0];
                goto compare;
              case 5:
                resword = &wordlist[1];
                goto compare;
              case 16:
                resword = &wordlist[2];
                goto compare;
              case 23:
                resword = &wordlist[3];
                goto compare;
              case 36:
                resword = &wordlist[4];
                goto compare;
              case 42:
                resword = &wordlist[5];
                goto compare;
              case 48:
                resword = &wordlist[6];
                goto compare;
              case 53:
                wordptr = &wordlist[7];
                wordendptr = wordptr + 2;
                goto multicompare;
              case 60:
                resword = &wordlist[9];
                goto compare;
              case 61:
                resword = &wordlist[10];
                goto compare;
              case 66:
                resword = &wordlist[11];
                goto compare;
              case 76:
                resword = &wordlist[12];
                goto compare;
              case 83:
                wordptr = &wordlist[13];
                wordendptr = wordptr + 2;
                goto multicompare;
              case 86:
                resword = &wordlist[15];
                goto compare;
              case 88:
                wordptr = &wordlist[16];
                wordendptr = wordptr + 2;
                goto multicompare;
              case 91:
                resword = &wordlist[18];
                goto compare;
              case 94:
                resword = &wordlist[19];
                goto compare;
              case 99:
                resword = &wordlist[20];
                goto compare;
              case 100:
                resword = &wordlist[21];
                goto compare;
              case 103:
                resword = &wordlist[22];
                goto compare;
              case 105:
                resword = &wordlist[23];
                goto compare;
              case 107:
                resword = &wordlist[24];
                goto compare;
              case 111:
                resword = &wordlist[25];
                goto compare;
              case 113:
                resword = &wordlist[26];
                goto compare;
              case 116:
                resword = &wordlist[27];
                goto compare;
              case 120:
                resword = &wordlist[28];
                goto compare;
              case 121:
                resword = &wordlist[29];
                goto compare;
              case 122:
                resword = &wordlist[30];
                goto compare;
              case 123:
                resword = &wordlist[31];
                goto compare;
              case 129:
                resword = &wordlist[32];
                goto compare;
              case 131:
                resword = &wordlist[33];
                goto compare;
              case 132:
                resword = &wordlist[34];
                goto compare;
              case 136:
                resword = &wordlist[35];
                goto compare;
              case 137:
                resword = &wordlist[36];
                goto compare;
              case 140:
                resword = &wordlist[37];
                goto compare;
              case 142:
                resword = &wordlist[38];
                goto compare;
              case 146:
                resword = &wordlist[39];
                goto compare;
              case 150:
                resword = &wordlist[40];
                goto compare;
              case 155:
                resword = &wordlist[41];
                goto compare;
              case 157:
                resword = &wordlist[42];
                goto compare;
              case 163:
                resword = &wordlist[43];
                goto compare;
              case 165:
                resword = &wordlist[44];
                goto compare;
              case 167:
                resword = &wordlist[45];
                goto compare;
              case 168:
                resword = &wordlist[46];
                goto compare;
              case 171:
                resword = &wordlist[47];
                goto compare;
              case 175:
                resword = &wordlist[48];
                goto compare;
              case 177:
                resword = &wordlist[49];
                goto compare;
              case 178:
                resword = &wordlist[50];
                goto compare;
              case 180:
                resword = &wordlist[51];
                goto compare;
              case 184:
                resword = &wordlist[52];
                goto compare;
              case 187:
                resword = &wordlist[53];
                goto compare;
              case 189:
                resword = &wordlist[54];
                goto compare;
              case 193:
                resword = &wordlist[55];
                goto compare;
              case 194:
                resword = &wordlist[56];
                goto compare;
              case 195:
                resword = &wordlist[57];
                goto compare;
              case 196:
                resword = &wordlist[58];
                goto compare;
              case 197:
                resword = &wordlist[59];
                goto compare;
              case 202:
                resword = &wordlist[60];
                goto compare;
              case 209:
                resword = &wordlist[61];
                goto compare;
              case 213:
                resword = &wordlist[62];
                goto compare;
              case 217:
                resword = &wordlist[63];
                goto compare;
              case 218:
                resword = &wordlist[64];
                goto compare;
              case 219:
                wordptr = &wordlist[65];
                wordendptr = wordptr + 2;
                goto multicompare;
              case 220:
                wordptr = &wordlist[67];
                wordendptr = wordptr + 2;
                goto multicompare;
              case 225:
                resword = &wordlist[69];
                goto compare;
              case 229:
                resword = &wordlist[70];
                goto compare;
              case 232:
                resword = &wordlist[71];
                goto compare;
              case 240:
                resword = &wordlist[72];
                goto compare;
              case 246:
                resword = &wordlist[73];
                goto compare;
              case 250:
                resword = &wordlist[74];
                goto compare;
              case 251:
                resword = &wordlist[75];
                goto compare;
              case 254:
                resword = &wordlist[76];
                goto compare;
              case 255:
                resword = &wordlist[77];
                goto compare;
              case 257:
                resword = &wordlist[78];
                goto compare;
              case 258:
                resword = &wordlist[79];
                goto compare;
              case 262:
                resword = &wordlist[80];
                goto compare;
              case 264:
                resword = &wordlist[81];
                goto compare;
              case 270:
                resword = &wordlist[82];
                goto compare;
              case 273:
                resword = &wordlist[83];
                goto compare;
              case 275:
                resword = &wordlist[84];
                goto compare;
              case 279:
                resword = &wordlist[85];
                goto compare;
              case 284:
                resword = &wordlist[86];
                goto compare;
              case 286:
                resword = &wordlist[87];
                goto compare;
              case 289:
                resword = &wordlist[88];
                goto compare;
              case 291:
                resword = &wordlist[89];
                goto compare;
              case 293:
                resword = &wordlist[90];
                goto compare;
              case 294:
                resword = &wordlist[91];
                goto compare;
              case 296:
                resword = &wordlist[92];
                goto compare;
              case 297:
                resword = &wordlist[93];
                goto compare;
              case 298:
                resword = &wordlist[94];
                goto compare;
              case 300:
                resword = &wordlist[95];
                goto compare;
              case 302:
                resword = &wordlist[96];
                goto compare;
              case 307:
                resword = &wordlist[97];
                goto compare;
              case 308:
                resword = &wordlist[98];
                goto compare;
              case 317:
                resword = &wordlist[99];
                goto compare;
              case 322:
                resword = &wordlist[100];
                goto compare;
              case 325:
                resword = &wordlist[101];
                goto compare;
              case 331:
                resword = &wordlist[102];
                goto compare;
              case 332:
                resword = &wordlist[103];
                goto compare;
              case 335:
                resword = &wordlist[104];
                goto compare;
              case 336:
                resword = &wordlist[105];
                goto compare;
              case 339:
                resword = &wordlist[106];
                goto compare;
              case 342:
                resword = &wordlist[107];
                goto compare;
              case 344:
                resword = &wordlist[108];
                goto compare;
              case 345:
                resword = &wordlist[109];
                goto compare;
              case 349:
                resword = &wordlist[110];
                goto compare;
              case 350:
                resword = &wordlist[111];
                goto compare;
              case 354:
                resword = &wordlist[112];
                goto compare;
              case 356:
                resword = &wordlist[113];
                goto compare;
              case 357:
                resword = &wordlist[114];
                goto compare;
              case 358:
                resword = &wordlist[115];
                goto compare;
              case 359:
                resword = &wordlist[116];
                goto compare;
              case 360:
                resword = &wordlist[117];
                goto compare;
              case 366:
                resword = &wordlist[118];
                goto compare;
              case 380:
                resword = &wordlist[119];
                goto compare;
              case 389:
                resword = &wordlist[120];
                goto compare;
              case 402:
                resword = &wordlist[121];
                goto compare;
              case 404:
                resword = &wordlist[122];
                goto compare;
              case 408:
                resword = &wordlist[123];
                goto compare;
              case 410:
                resword = &wordlist[124];
                goto compare;
              case 411:
                resword = &wordlist[125];
                goto compare;
              case 415:
                resword = &wordlist[126];
                goto compare;
              case 416:
                resword = &wordlist[127];
                goto compare;
              case 422:
                resword = &wordlist[128];
                goto compare;
              case 423:
                resword = &wordlist[129];
                goto compare;
              case 426:
                resword = &wordlist[130];
                goto compare;
              case 427:
                resword = &wordlist[131];
                goto compare;
              case 428:
                resword = &wordlist[132];
                goto compare;
              case 433:
                resword = &wordlist[133];
                goto compare;
              case 436:
                resword = &wordlist[134];
                goto compare;
              case 438:
                resword = &wordlist[135];
                goto compare;
              case 439:
                resword = &wordlist[136];
                goto compare;
              case 441:
                resword = &wordlist[137];
                goto compare;
              case 444:
                wordptr = &wordlist[138];
                wordendptr = wordptr + 2;
                goto multicompare;
              case 445:
                resword = &wordlist[140];
                goto compare;
              case 453:
                resword = &wordlist[141];
                goto compare;
              case 454:
                resword = &wordlist[142];
                goto compare;
              case 455:
                resword = &wordlist[143];
                goto compare;
              case 456:
                resword = &wordlist[144];
                goto compare;
              case 459:
                resword = &wordlist[145];
                goto compare;
              case 468:
                resword = &wordlist[146];
                goto compare;
              case 476:
                resword = &wordlist[147];
                goto compare;
              case 479:
                resword = &wordlist[148];
                goto compare;
              case 480:
                resword = &wordlist[149];
                goto compare;
              case 481:
                resword = &wordlist[150];
                goto compare;
              case 482:
                resword = &wordlist[151];
                goto compare;
              case 484:
                resword = &wordlist[152];
                goto compare;
              case 487:
                resword = &wordlist[153];
                goto compare;
              case 491:
                resword = &wordlist[154];
                goto compare;
              case 492:
                resword = &wordlist[155];
                goto compare;
              case 498:
                resword = &wordlist[156];
                goto compare;
              case 505:
                resword = &wordlist[157];
                goto compare;
              case 506:
                resword = &wordlist[158];
                goto compare;
              case 514:
                resword = &wordlist[159];
                goto compare;
              case 533:
                resword = &wordlist[160];
                goto compare;
              case 536:
                resword = &wordlist[161];
                goto compare;
              case 539:
                resword = &wordlist[162];
                goto compare;
              case 540:
                resword = &wordlist[163];
                goto compare;
              case 542:
                resword = &wordlist[164];
                goto compare;
              case 544:
                resword = &wordlist[165];
                goto compare;
              case 547:
                resword = &wordlist[166];
                goto compare;
              case 550:
                resword = &wordlist[167];
                goto compare;
              case 551:
                resword = &wordlist[168];
                goto compare;
              case 561:
                resword = &wordlist[169];
                goto compare;
              case 567:
                resword = &wordlist[170];
                goto compare;
              case 569:
                resword = &wordlist[171];
                goto compare;
              case 576:
                resword = &wordlist[172];
                goto compare;
              case 580:
                resword = &wordlist[173];
                goto compare;
              case 583:
                resword = &wordlist[174];
                goto compare;
              case 584:
                resword = &wordlist[175];
                goto compare;
              case 585:
                resword = &wordlist[176];
                goto compare;
              case 589:
                resword = &wordlist[177];
                goto compare;
              case 592:
                resword = &wordlist[178];
                goto compare;
              case 593:
                resword = &wordlist[179];
                goto compare;
              case 596:
                resword = &wordlist[180];
                goto compare;
              case 597:
                resword = &wordlist[181];
                goto compare;
              case 600:
                resword = &wordlist[182];
                goto compare;
              case 610:
                resword = &wordlist[183];
                goto compare;
              case 611:
                resword = &wordlist[184];
                goto compare;
              case 615:
                resword = &wordlist[185];
                goto compare;
              case 616:
                resword = &wordlist[186];
                goto compare;
              case 617:
                resword = &wordlist[187];
                goto compare;
              case 621:
                resword = &wordlist[188];
                goto compare;
              case 629:
                resword = &wordlist[189];
                goto compare;
              case 632:
                resword = &wordlist[190];
                goto compare;
              case 634:
                resword = &wordlist[191];
                goto compare;
              case 643:
                resword = &wordlist[192];
                goto compare;
              case 645:
                resword = &wordlist[193];
                goto compare;
              case 654:
                resword = &wordlist[194];
                goto compare;
              case 657:
                resword = &wordlist[195];
                goto compare;
              case 663:
                resword = &wordlist[196];
                goto compare;
              case 665:
                resword = &wordlist[197];
                goto compare;
              case 675:
                resword = &wordlist[198];
                goto compare;
              case 678:
                resword = &wordlist[199];
                goto compare;
              case 684:
                resword = &wordlist[200];
                goto compare;
              case 690:
                resword = &wordlist[201];
                goto compare;
              case 700:
                resword = &wordlist[202];
                goto compare;
              case 702:
                resword = &wordlist[203];
                goto compare;
              case 710:
                resword = &wordlist[204];
                goto compare;
              case 713:
                resword = &wordlist[205];
                goto compare;
              case 720:
                resword = &wordlist[206];
                goto compare;
              case 723:
                resword = &wordlist[207];
                goto compare;
              case 724:
                resword = &wordlist[208];
                goto compare;
              case 738:
                resword = &wordlist[209];
                goto compare;
              case 741:
                resword = &wordlist[210];
                goto compare;
              case 743:
                resword = &wordlist[211];
                goto compare;
              case 744:
                resword = &wordlist[212];
                goto compare;
              case 749:
                resword = &wordlist[213];
                goto compare;
              case 751:
                resword = &wordlist[214];
                goto compare;
              case 755:
                resword = &wordlist[215];
                goto compare;
              case 761:
                resword = &wordlist[216];
                goto compare;
              case 764:
                resword = &wordlist[217];
                goto compare;
              case 766:
                resword = &wordlist[218];
                goto compare;
              case 768:
                resword = &wordlist[219];
                goto compare;
              case 769:
                resword = &wordlist[220];
                goto compare;
              case 770:
                resword = &wordlist[221];
                goto compare;
              case 772:
                resword = &wordlist[222];
                goto compare;
              case 784:
                resword = &wordlist[223];
                goto compare;
              case 800:
                resword = &wordlist[224];
                goto compare;
              case 807:
                resword = &wordlist[225];
                goto compare;
              case 808:
                resword = &wordlist[226];
                goto compare;
              case 823:
                resword = &wordlist[227];
                goto compare;
              case 826:
                resword = &wordlist[228];
                goto compare;
              case 827:
                resword = &wordlist[229];
                goto compare;
              case 839:
                resword = &wordlist[230];
                goto compare;
              case 842:
                resword = &wordlist[231];
                goto compare;
              case 868:
                resword = &wordlist[232];
                goto compare;
              case 902:
                resword = &wordlist[233];
                goto compare;
              case 903:
                resword = &wordlist[234];
                goto compare;
              case 905:
                resword = &wordlist[235];
                goto compare;
              case 919:
                resword = &wordlist[236];
                goto compare;
              case 924:
                resword = &wordlist[237];
                goto compare;
              case 926:
                resword = &wordlist[238];
                goto compare;
              case 937:
                resword = &wordlist[239];
                goto compare;
              case 940:
                resword = &wordlist[240];
                goto compare;
              case 975:
                resword = &wordlist[241];
                goto compare;
              case 979:
                resword = &wordlist[242];
                goto compare;
              case 982:
                resword = &wordlist[243];
                goto compare;
              case 997:
                resword = &wordlist[244];
                goto compare;
              case 1000:
                resword = &wordlist[245];
                goto compare;
              case 1011:
                resword = &wordlist[246];
                goto compare;
              case 1012:
                resword = &wordlist[247];
                goto compare;
              case 1016:
                resword = &wordlist[248];
                goto compare;
              case 1028:
                resword = &wordlist[249];
                goto compare;
              case 1029:
                resword = &wordlist[250];
                goto compare;
              case 1032:
                resword = &wordlist[251];
                goto compare;
              case 1061:
                resword = &wordlist[252];
                goto compare;
              case 1070:
                resword = &wordlist[253];
                goto compare;
              case 1075:
                resword = &wordlist[254];
                goto compare;
              case 1079:
                resword = &wordlist[255];
                goto compare;
              case 1097:
                resword = &wordlist[256];
                goto compare;
              case 1098:
                resword = &wordlist[257];
                goto compare;
              case 1102:
                resword = &wordlist[258];
                goto compare;
              case 1131:
                resword = &wordlist[259];
                goto compare;
              case 1145:
                resword = &wordlist[260];
                goto compare;
              case 1155:
                resword = &wordlist[261];
                goto compare;
              case 1158:
                resword = &wordlist[262];
                goto compare;
              case 1160:
                resword = &wordlist[263];
                goto compare;
              case 1161:
                resword = &wordlist[264];
                goto compare;
              case 1175:
                resword = &wordlist[265];
                goto compare;
              case 1187:
                resword = &wordlist[266];
                goto compare;
              case 1200:
                resword = &wordlist[267];
                goto compare;
              case 1209:
                resword = &wordlist[268];
                goto compare;
              case 1210:
                resword = &wordlist[269];
                goto compare;
              case 1220:
                resword = &wordlist[270];
                goto compare;
              case 1235:
                resword = &wordlist[271];
                goto compare;
              case 1264:
                resword = &wordlist[272];
                goto compare;
              case 1267:
                resword = &wordlist[273];
                goto compare;
              case 1276:
                resword = &wordlist[274];
                goto compare;
              case 1294:
                resword = &wordlist[275];
                goto compare;
              case 1295:
                resword = &wordlist[276];
                goto compare;
              case 1314:
                resword = &wordlist[277];
                goto compare;
              case 1317:
                resword = &wordlist[278];
                goto compare;
              case 1332:
                resword = &wordlist[279];
                goto compare;
              case 1335:
                resword = &wordlist[280];
                goto compare;
              case 1338:
                resword = &wordlist[281];
                goto compare;
              case 1365:
                resword = &wordlist[282];
                goto compare;
              case 1415:
                resword = &wordlist[283];
                goto compare;
              case 1441:
                resword = &wordlist[284];
                goto compare;
              case 1539:
                resword = &wordlist[285];
                goto compare;
              case 1599:
                resword = &wordlist[286];
                goto compare;
              case 1647:
                resword = &wordlist[287];
                goto compare;
              case 1758:
                resword = &wordlist[288];
                goto compare;
              case 1801:
                resword = &wordlist[289];
                goto compare;
              case 1868:
                resword = &wordlist[290];
                goto compare;
              case 1870:
                resword = &wordlist[291];
                goto compare;
              case 1929:
                resword = &wordlist[292];
                goto compare;
              case 1982:
                resword = &wordlist[293];
                goto compare;
              case 2146:
                resword = &wordlist[294];
                goto compare;
              case 2217:
                resword = &wordlist[295];
                goto compare;
              case 2376:
                resword = &wordlist[296];
                goto compare;
              case 2441:
                resword = &wordlist[297];
                goto compare;
              case 2484:
                resword = &wordlist[298];
                goto compare;
              case 2814:
                resword = &wordlist[299];
                goto compare;
            }
          return 0;
        multicompare:
          while (wordptr < wordendptr)
            {
              register const char *s = wordptr->name;

              if (*str == *s && !strcmp (str + 1, s + 1))
                return wordptr;
              wordptr++;
            }
          return 0;
        compare:
          {
            register const char *s = resword->name;

            if (*str == *s && !strcmp (str + 1, s + 1))
              return resword;
          }
        }
    }
  return 0;
}
