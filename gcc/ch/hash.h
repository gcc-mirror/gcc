/* C code produced by gperf version 2.5 (GNU C++ version) */
/* Command-line: gperf -D -E -S1 -p -j1 -i 1 -g -o -t -k* gperf.tmp  */
struct resword {
  char        *name;
  short        token;
  enum rid     rid;
  enum toktype { RESERVED, DIRECTIVE, PREDEF } flags;
};
extern tree ridpointers [];
/* maximum key range = 2815, duplicates = 6 */

#ifdef __GNUC__
inline
#endif
static unsigned int
hash (str, len)
     register char *str;
     register int unsigned len;
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
       10,   56,   40, 2822, 2822, 2822, 2822, 2822,
    };
  register int hval = len;

  switch (hval)
    {
      default:
      case 30:
        hval += asso_values[str[29]];
      case 29:
        hval += asso_values[str[28]];
      case 28:
        hval += asso_values[str[27]];
      case 27:
        hval += asso_values[str[26]];
      case 26:
        hval += asso_values[str[25]];
      case 25:
        hval += asso_values[str[24]];
      case 24:
        hval += asso_values[str[23]];
      case 23:
        hval += asso_values[str[22]];
      case 22:
        hval += asso_values[str[21]];
      case 21:
        hval += asso_values[str[20]];
      case 20:
        hval += asso_values[str[19]];
      case 19:
        hval += asso_values[str[18]];
      case 18:
        hval += asso_values[str[17]];
      case 17:
        hval += asso_values[str[16]];
      case 16:
        hval += asso_values[str[15]];
      case 15:
        hval += asso_values[str[14]];
      case 14:
        hval += asso_values[str[13]];
      case 13:
        hval += asso_values[str[12]];
      case 12:
        hval += asso_values[str[11]];
      case 11:
        hval += asso_values[str[10]];
      case 10:
        hval += asso_values[str[9]];
      case 9:
        hval += asso_values[str[8]];
      case 8:
        hval += asso_values[str[7]];
      case 7:
        hval += asso_values[str[6]];
      case 6:
        hval += asso_values[str[5]];
      case 5:
        hval += asso_values[str[4]];
      case 4:
        hval += asso_values[str[3]];
      case 3:
        hval += asso_values[str[2]];
      case 2:
        hval += asso_values[str[1]];
      case 1:
        hval += asso_values[str[0]];
    }
  return hval;
}

#ifdef __GNUC__
inline
#endif
struct resword *
in_word_set (str, len)
     register char *str;
     register unsigned int len;
{
  enum
    {
      TOTAL_KEYWORDS = 300,
      MIN_WORD_LENGTH = 2,
      MAX_WORD_LENGTH = 30,
      MIN_HASH_VALUE = 7,
      MAX_HASH_VALUE = 2821,
    };

  static struct resword wordlist[] =
    {
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"AT", 			AT,			NORID,	RESERVED},
      {"WITH", 			WITH,			NORID,	RESERVED},
      {"THIS", 			THIS,			NORID,	RESERVED},
      {"else", 			ELSE,			NORID,	RESERVED},
      {"while", 			WHILE,			NORID,	RESERVED},
      {"TO", 			TO,			NORID,	RESERVED},
      {"seize", 			SEIZE,			NORID,	RESERVED},
      {"DO", 			DO,			NORID,	RESERVED},
      {"OD", 			OD,			NORID,	RESERVED},
      {"BIT", 			BOOLS,			RID_BOOLS,	PREDEF},
      {"IN", 			IN,			RID_IN,	RESERVED},
      {"INIT", 			INIT,			NORID,	RESERVED},
      {"AND", 			AND,			NORID,	RESERVED},
      {"fi", 			FI,			NORID,	RESERVED},
      {"if", 			IF,			NORID,	RESERVED},
      {"set", 			SET,			NORID,	RESERVED},
      {"FI", 			FI,			NORID,	RESERVED},
      {"IF", 			IF,			NORID,	RESERVED},
      {"by", 			BY,			NORID,	RESERVED},
      {"this", 			THIS,			NORID,	RESERVED},
      {"with", 			WITH,			NORID,	RESERVED},
      {"STATIC", 			STATIC,			NORID,	RESERVED},
      {"exit", 			EXIT,			NORID,	RESERVED},
      {"ON", 			ON,			NORID,	RESERVED},
      {"NOT", 			NOT,			NORID,	RESERVED},
      {"elsif", 			ELSIF,			NORID,	RESERVED},
      {"START", 			START,			NORID,	RESERVED},
      {"list", 			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"POS", 			POS,			NORID,	RESERVED},
      {"DOWN", 			DOWN,			NORID,	RESERVED},
      {"STOP", 			STOP,			NORID,	RESERVED},
      {"BIN", 			BIN,			NORID,	RESERVED},
      {"GOTO", 			GOTO,			NORID,	RESERVED},
      {"bit", 			BOOLS,			RID_BOOLS,	PREDEF},
      {"OF", 			OF,			NORID,	RESERVED},
      {"all", 			ALL,			NORID,	RESERVED},
      {"OR", 			OR,			NORID,	RESERVED},
      {"ROW", 			ROW,			NORID,	RESERVED},
      {"LIST", 			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"XOR", 			XOR,			NORID,	RESERVED},
      {"PACK", 			PACK,			NORID,	RESERVED},
      {"based", 			BASED,			NORID,	RESERVED},
      {"step", 			STEP,			NORID,	RESERVED},
      {"page", 			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"asm", 			ASM_KEYWORD,		NORID,	RESERVED},
      {"dcl", 			DCL,			NORID,	RESERVED},
      {"ASM", 			ASM_KEYWORD,		NORID,	RESERVED},
      {"ANDIF", 			ANDIF,			NORID,	RESERVED},
      {"simple", 			SIMPLE,			NORID,	RESERVED},
      {"at", 			AT,			NORID,	RESERVED},
      {"OUT", 			PARAMATTR,		RID_OUT,	RESERVED},
      {"BY", 			BY,			NORID,	RESERVED},
      {"text", 			TEXT,			NORID,	RESERVED},
      {"FAR", 			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"up", 			UP,			NORID,	RESERVED},
      {"delay", 			DELAY,			NORID,	RESERVED},
      {"CHARS", 			CHARS,			NORID,	RESERVED},
      {"UP", 			UP,			NORID,	RESERVED},
      {"spec", 			SPEC,			NORID,	RESERVED},
      {"SYN", 			SYN,			NORID,	RESERVED},
      {"GRANT", 			GRANT,			NORID,	RESERVED},
      {"MOD", 			MOD,			NORID,	RESERVED},
      {"small", 			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"DCL", 			DCL,			NORID,	RESERVED},
      {"ever", 			EVER,			NORID,	RESERVED},
      {"do", 			DO,			NORID,	RESERVED},
      {"od", 			OD,			NORID,	RESERVED},
      {"case", 			CASE,			NORID,	RESERVED},
      {"esac", 			ESAC,			NORID,	RESERVED},
      {"CCITT_OS", 		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"FOR", 			FOR,			NORID,	RESERVED},
      {"ORIF", 			ORIF,			NORID,	RESERVED},
      {"BODY", 			BODY,			NORID,	RESERVED},
      {"INOUT", 			PARAMATTR,		RID_INOUT,	RESERVED},
      {"SIGNAL", 			SIGNAL,			NORID,	RESERVED},
      {"LOC", 			LOC,			NORID,	RESERVED},
      {"NOLIST", 			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"even", 			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"in", 			IN,			RID_IN,	RESERVED},
      {"ALL", 			ALL,			NORID,	RESERVED},
      {"NOPACK", 			NOPACK,			NORID,	RESERVED},
      {"call", 			CALL,			NORID,	RESERVED},
      {"pos", 			POS,			NORID,	RESERVED},
      {"end", 			END,			NORID,	RESERVED},
      {"send", 			SEND,			NORID,	RESERVED},
      {"of", 			OF,			NORID,	RESERVED},
      {"PROC", 			PROC,			NORID,	RESERVED},
      {"to", 			TO,			NORID,	RESERVED},
      {"rem", 			REM,			NORID,	RESERVED},
      {"pack", 			PACK,			NORID,	RESERVED},
      {"BOOLS", 			BOOLS,			RID_BOOLS,	RESERVED},
      {"mod", 			MOD,			NORID,	RESERVED},
      {"ref", 			REF,			NORID,	RESERVED},
      {"use_seize_file", 		USE_SEIZE_FILE,		NORID,	DIRECTIVE},
      {"bin", 			BIN,			NORID,	RESERVED},
      {"medium", 			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"begin", 			BEGINTOKEN,		NORID,	RESERVED},
      {"FORBID", 			FORBID,			NORID,	RESERVED},
      {"syn", 			SYN,			NORID,	RESERVED},
      {"body", 			BODY,			NORID,	RESERVED},
      {"ARRAY", 			ARRAY,			NORID,	RESERVED},
      {"STRUCT", 			STRUCT,			NORID,	RESERVED},
      {"read", 			READ,			RID_READ,	RESERVED},
      {"cycle", 			CYCLE,			NORID,	RESERVED},
      {"large", 			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"VARYING", 		VARYING,		NORID,	RESERVED},
      {"CALL", 			CALL,			NORID,	RESERVED},
      {"then", 			THEN,			NORID,	RESERVED},
      {"event", 			EVENT,			NORID,	RESERVED},
      {"cause", 			CAUSE,			NORID,	RESERVED},
      {"loc", 			LOC,			NORID,	RESERVED},
      {"access", 			ACCESS,			NORID,	RESERVED},
      {"init", 			INIT,			NORID,	RESERVED},
      {"receive", 		RECEIVE,		NORID,	RESERVED},
      {"TEXT", 			TEXT,			NORID,	RESERVED},
      {"EXIT", 			EXIT,			NORID,	RESERVED},
      {"stop", 			STOP,			NORID,	RESERVED},
      {"SET", 			SET,			NORID,	RESERVED},
      {"and", 			AND,			NORID,	RESERVED},
      {"signal", 			SIGNAL,			NORID,	RESERVED},
      {"far", 			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"assert", 			ASSERT,			NORID,	RESERVED},
      {"static", 			STATIC,			NORID,	RESERVED},
      {"debug_types", 		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"prefixed", 		PREFIXED,		NORID,	RESERVED},
      {"out", 			PARAMATTR,		RID_OUT,	RESERVED},
      {"THEN", 			THEN,			NORID,	RESERVED},
      {"or", 			OR,			NORID,	RESERVED},
      {"END", 			END,			NORID,	RESERVED},
      {"row", 			ROW,			NORID,	RESERVED},
      {"STEP", 			STEP,			NORID,	RESERVED},
      {"xor", 			XOR,			NORID,	RESERVED},
      {"SMALL", 			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"PRIORITY", 		PRIORITY,		NORID,	RESERVED},
      {"SEND", 			SEND,			NORID,	RESERVED},
      {"BASED", 			BASED,			NORID,	RESERVED},
      {"chars", 			CHARS,			NORID,	RESERVED},
      {"DYNAMIC", 		DYNAMIC,		RID_DYNAMIC,	RESERVED},
      {"CASE", 			CASE,			NORID,	RESERVED},
      {"ESAC", 			ESAC,			NORID,	RESERVED},
      {"module", 			MODULE,			NORID,	RESERVED},
      {"on", 			ON,			NORID,	RESERVED},
      {"result", 			RESULT,			NORID,	RESERVED},
      {"PAGE", 			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"andif", 			ANDIF,			NORID,	RESERVED},
      {"READ", 			READ,			RID_READ,	RESERVED},
      {"bools", 			BOOLS,			RID_BOOLS,	RESERVED},
      {"ASSERT", 			ASSERT,			NORID,	RESERVED},
      {"debug_lines", 		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"after", 			AFTER,			NORID,	RESERVED},
      {"ALL_STATIC_ON", 		ALL_STATIC_ON,		NORID,	DIRECTIVE},
      {"down", 			DOWN,			NORID,	RESERVED},
      {"WHILE", 			WHILE,			NORID,	RESERVED},
      {"start", 			START,			NORID,	RESERVED},
      {"optimize", 		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"goto", 			GOTO,			NORID,	RESERVED},
      {"for", 			FOR,			NORID,	RESERVED},
      {"SPEC", 			SPEC,			NORID,	RESERVED},
      {"orif", 			ORIF,			NORID,	RESERVED},
      {"BEGIN", 			BEGINTOKEN,		NORID,	RESERVED},
      {"REF", 			REF,			NORID,	RESERVED},
      {"OPTIMIZATION_WINDOW", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"ACCESS", 			ACCESS,			NORID,	RESERVED},
      {"AFTER", 			AFTER,			NORID,	RESERVED},
      {"not", 			NOT,			NORID,	RESERVED},
      {"buffer", 			BUFFER,			NORID,	RESERVED},
      {"inline", 			INLINE,			RID_INLINE,	RESERVED},
      {"CONTEXT", 		CONTEXT,		NORID,	RESERVED},
      {"RANGE", 			RANGE,			NORID,	RESERVED},
      {"newmode", 		NEWMODE,		NORID,	RESERVED},
      {"range", 			RANGE,			NORID,	RESERVED},
      {"forbid", 			FORBID,			NORID,	RESERVED},
      {"nolist", 			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"CAUSE", 			CAUSE,			NORID,	RESERVED},
      {"ELSIF", 			ELSIF,			NORID,	RESERVED},
      {"remote", 			REMOTE,			NORID,	RESERVED},
      {"timeout", 		TIMEOUT,		NORID,	RESERVED},
      {"powerset", 		POWERSET,		NORID,	RESERVED},
      {"debug_symbols", 		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"general", 		GENERAL,		NORID,	RESERVED},
      {"REGION", 			REGION,			NORID,	RESERVED},
      {"REM", 			REM,			NORID,	RESERVED},
      {"ALL_STATIC_OFF", 		ALL_STATIC_OFF,		NORID,  DIRECTIVE},
      {"INLINE", 			INLINE,			RID_INLINE,	RESERVED},
      {"synmode", 		SYNMODE,		NORID,	RESERVED},
      {"proc", 			PROC,			NORID,	RESERVED},
      {"LARGE", 			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"DELAY", 			DELAY,			NORID,	RESERVED},
      {"process", 		PROCESS,		NORID,	RESERVED},
      {"OPTIMIZE", 		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"varying", 		VARYING,		NORID,	RESERVED},
      {"dynamic", 		DYNAMIC,		RID_DYNAMIC,	RESERVED},
      {"ccitt_os", 		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"struct", 			STRUCT,			NORID,	RESERVED},
      {"grant", 			GRANT,			NORID,	RESERVED},
      {"empty_off", 		EMPTY_OFF,		NORID,	DIRECTIVE},
      {"PROCESS", 		PROCESS,		NORID,	RESERVED},
      {"RANGE_ON", 		RANGE_ON,		NORID,	DIRECTIVE},
      {"inout", 			PARAMATTR,		RID_INOUT,	RESERVED},
      {"array", 			ARRAY,			NORID,	RESERVED},
      {"region", 			REGION,			NORID,	RESERVED},
      {"TIMEOUT", 		TIMEOUT,		NORID,	RESERVED},
      {"recursive", 		RECURSIVE,		NORID,	RESERVED},
      {"event_code", 		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"NONREF", 			NONREF,			NORID,	RESERVED},
      {"SIMPLE", 			SIMPLE,			NORID,	RESERVED},
      {"SEIZE", 			SEIZE,			NORID,	RESERVED},
      {"RESULT", 			RESULT,			NORID,	RESERVED},
      {"multiple_data_segs", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"signal_code", 		SIGNAL_CODE,		NORID,	DIRECTIVE},
      {"RETURN", 			RETURN,			NORID,	RESERVED},
      {"CONTINUE", 		CONTINUE,		NORID,	RESERVED},
      {"SIGNAL_CODE", 		SIGNAL_CODE,		NORID,	DIRECTIVE},
      {"empty_on", 		EMPTY_ON,		NORID,	DIRECTIVE},
      {"nopack", 			NOPACK,			NORID,	RESERVED},
      {"RETURNS", 		RETURNS,		NORID,	RESERVED},
      {"CYCLE", 			CYCLE,			NORID,	RESERVED},
      {"SYNMODE", 		SYNMODE,		NORID,	RESERVED},
      {"exceptions", 		EXCEPTIONS,		NORID,	RESERVED},
      {"EVEN", 			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"PRINT_O_CODE", 		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"EVENT", 			EVENT,			NORID,	RESERVED},
      {"context", 		CONTEXT,		NORID,	RESERVED},
      {"RANGE_OFF", 		RANGE_OFF,		NORID,	DIRECTIVE},
      {"EVER", 			EVER,			NORID,	RESERVED},
      {"EMPTY_ON", 		EMPTY_ON,		NORID,	DIRECTIVE},
      {"MEDIUM", 			IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"BUFFER", 			BUFFER,			NORID,	RESERVED},
      {"MODULE", 			MODULE,			NORID,	RESERVED},
      {"grant_file_size", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"ELSE", 			ELSE,			NORID,	RESERVED},
      {"process_type", 		PROCESS_TYPE_TOKEN,	NORID,	DIRECTIVE},
      {"priority", 		PRIORITY,		NORID,	RESERVED},
      {"buffer_code", 		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"return", 			RETURN,			NORID,	RESERVED},
      {"returns", 		RETURNS,		NORID,	RESERVED},
      {"all_static_off", 		ALL_STATIC_OFF,		NORID,  DIRECTIVE},
      {"POWERSET", 		POWERSET,		NORID,	RESERVED},
      {"EMPTY_OFF", 		EMPTY_OFF,		NORID,	DIRECTIVE},
      {"range_off", 		RANGE_OFF,		NORID,	DIRECTIVE},
      {"signal_max_length", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"PREFIXED", 		PREFIXED,		NORID,	RESERVED},
      {"NEWMODE", 		NEWMODE,		NORID,	RESERVED},
      {"EXCEPTIONS", 		EXCEPTIONS,		NORID,	RESERVED},
      {"REMOTE", 			REMOTE,			NORID,	RESERVED},
      {"SHORT_PRED_SUCC", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"all_static_on", 		ALL_STATIC_ON,		NORID,	DIRECTIVE},
      {"nonref", 			NONREF,			NORID,	RESERVED},
      {"SIGNAL_MAX_LENGTH", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"REENTRANT", 		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"range_on", 		RANGE_ON,		NORID,	DIRECTIVE},
      {"GENERAL", 		GENERAL,		NORID,	RESERVED},
      {"continue", 		CONTINUE,		NORID,	RESERVED},
      {"STATE_ROUTINE", 		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"EXTRA_CONST_SEG", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"use_seize_file_restricted", 	USE_SEIZE_FILE_RESTRICTED,	NORID,	DIRECTIVE},
      {"ONLY_FOR_TARGET", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"extra_const_seg", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"multiple_const_segs", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"RECURSIVE", 		RECURSIVE,		NORID,	RESERVED},
      {"DEBUG_SYMBOLS", 		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"DEBUG_TYPES", 		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"GRANT_FILE_SIZE", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"DEBUG_LINES", 		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"ONLY_FOR_SIMULATION", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"state_routine", 		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"generate_set_names", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"print_o_code", 		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"PROCESS_TYPE", 		PROCESS_TYPE_TOKEN,	NORID,	DIRECTIVE},
      {"short_pred_succ", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"reentrant", 		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"RECEIVE", 		RECEIVE,		NORID,	RESERVED},
      {"EVENT_CODE", 		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"optimize_runtime", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"SUPPORT_CAUSING_ADDRESS", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"print_symbol_table", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"REENTRANT_ALL", 		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"PRINT_SYMBOL_TABLE", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"BUFFER_CODE", 		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"generate_all_set_names", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"NO_OVERLAP_CHECK", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"no_overlap_check", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"reentrant_all", 		IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"MULTIPLE_DATA_SEGS", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"OPTIMIZE_RUNTIME", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"only_for_target", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"MULTIPLE_CONST_SEGS", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"optimization_window", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"support_causing_address", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"USE_SEIZE_FILE", 		USE_SEIZE_FILE,		NORID,	DIRECTIVE},
      {"SEND_SIGNAL_DEFAULT_PRIORITY", 	SEND_SIGNAL_DEFAULT_PRIORITY,	NORID,	DIRECTIVE},
      {"make_publics_for_discrete_syns", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"only_for_simulation", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"send_signal_default_priority", 	SEND_SIGNAL_DEFAULT_PRIORITY,	NORID,	DIRECTIVE},
      {"send_buffer_default_priority", 	SEND_BUFFER_DEFAULT_PRIORITY,	NORID,	DIRECTIVE},
      {"GENERATE_SET_NAMES", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"MAKE_PUBLICS_FOR_DISCRETE_SYNS", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"SEND_BUFFER_DEFAULT_PRIORITY", 	SEND_BUFFER_DEFAULT_PRIORITY,	NORID,	DIRECTIVE},
      {"GENERATE_ALL_SET_NAMES", 	IGNORED_DIRECTIVE,	NORID,	DIRECTIVE},
      {"USE_SEIZE_FILE_RESTRICTED", 	USE_SEIZE_FILE_RESTRICTED,	NORID,	DIRECTIVE},
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= MIN_HASH_VALUE)
        {
            {
              struct resword  *resword; 

              switch (key)
                {
                case     7:
                  resword = &wordlist[7]; break;
                case    12:
                  resword = &wordlist[8]; break;
                case    23:
                  resword = &wordlist[9]; break;
                case    30:
                  resword = &wordlist[10]; break;
                case    43:
                  resword = &wordlist[11]; break;
                case    49:
                  resword = &wordlist[12]; break;
                case    55:
                  resword = &wordlist[13]; break;
                case    60:
                  resword = &wordlist[14];
                  if (*str == *resword->name && !strcmp (str + 1, resword->name + 1)) return resword;
                  resword = &wordlist[15];
                  if (*str == *resword->name && !strcmp (str + 1, resword->name + 1)) return resword;
                  return 0;
                case    67:
                  resword = &wordlist[16]; break;
                case    68:
                  resword = &wordlist[17]; break;
                case    73:
                  resword = &wordlist[18]; break;
                case    83:
                  resword = &wordlist[19]; break;
                case    90:
                  resword = &wordlist[20];
                  if (*str == *resword->name && !strcmp (str + 1, resword->name + 1)) return resword;
                  resword = &wordlist[21];
                  if (*str == *resword->name && !strcmp (str + 1, resword->name + 1)) return resword;
                  return 0;
                case    93:
                  resword = &wordlist[22]; break;
                case    95:
                  resword = &wordlist[23];
                  if (*str == *resword->name && !strcmp (str + 1, resword->name + 1)) return resword;
                  resword = &wordlist[24];
                  if (*str == *resword->name && !strcmp (str + 1, resword->name + 1)) return resword;
                  return 0;
                case    98:
                  resword = &wordlist[25]; break;
                case   101:
                  resword = &wordlist[26]; break;
                case   106:
                  resword = &wordlist[27]; break;
                case   107:
                  resword = &wordlist[28]; break;
                case   110:
                  resword = &wordlist[29]; break;
                case   112:
                  resword = &wordlist[30]; break;
                case   114:
                  resword = &wordlist[31]; break;
                case   118:
                  resword = &wordlist[32]; break;
                case   120:
                  resword = &wordlist[33]; break;
                case   123:
                  resword = &wordlist[34]; break;
                case   127:
                  resword = &wordlist[35]; break;
                case   128:
                  resword = &wordlist[36]; break;
                case   129:
                  resword = &wordlist[37]; break;
                case   130:
                  resword = &wordlist[38]; break;
                case   136:
                  resword = &wordlist[39]; break;
                case   138:
                  resword = &wordlist[40]; break;
                case   139:
                  resword = &wordlist[41]; break;
                case   143:
                  resword = &wordlist[42]; break;
                case   144:
                  resword = &wordlist[43]; break;
                case   147:
                  resword = &wordlist[44]; break;
                case   149:
                  resword = &wordlist[45]; break;
                case   153:
                  resword = &wordlist[46]; break;
                case   157:
                  resword = &wordlist[47]; break;
                case   162:
                  resword = &wordlist[48]; break;
                case   164:
                  resword = &wordlist[49]; break;
                case   170:
                  resword = &wordlist[50]; break;
                case   172:
                  resword = &wordlist[51]; break;
                case   174:
                  resword = &wordlist[52]; break;
                case   175:
                  resword = &wordlist[53]; break;
                case   178:
                  resword = &wordlist[54]; break;
                case   182:
                  resword = &wordlist[55]; break;
                case   184:
                  resword = &wordlist[56]; break;
                case   185:
                  resword = &wordlist[57]; break;
                case   187:
                  resword = &wordlist[58]; break;
                case   191:
                  resword = &wordlist[59]; break;
                case   194:
                  resword = &wordlist[60]; break;
                case   196:
                  resword = &wordlist[61]; break;
                case   200:
                  resword = &wordlist[62]; break;
                case   201:
                  resword = &wordlist[63]; break;
                case   202:
                  resword = &wordlist[64]; break;
                case   203:
                  resword = &wordlist[65]; break;
                case   204:
                  resword = &wordlist[66]; break;
                case   209:
                  resword = &wordlist[67]; break;
                case   216:
                  resword = &wordlist[68]; break;
                case   220:
                  resword = &wordlist[69]; break;
                case   224:
                  resword = &wordlist[70]; break;
                case   225:
                  resword = &wordlist[71]; break;
                case   226:
                  resword = &wordlist[72];
                  if (*str == *resword->name && !strcmp (str + 1, resword->name + 1)) return resword;
                  resword = &wordlist[73];
                  if (*str == *resword->name && !strcmp (str + 1, resword->name + 1)) return resword;
                  return 0;
                case   227:
                  resword = &wordlist[74];
                  if (*str == *resword->name && !strcmp (str + 1, resword->name + 1)) return resword;
                  resword = &wordlist[75];
                  if (*str == *resword->name && !strcmp (str + 1, resword->name + 1)) return resword;
                  return 0;
                case   232:
                  resword = &wordlist[76]; break;
                case   236:
                  resword = &wordlist[77]; break;
                case   239:
                  resword = &wordlist[78]; break;
                case   247:
                  resword = &wordlist[79]; break;
                case   253:
                  resword = &wordlist[80]; break;
                case   257:
                  resword = &wordlist[81]; break;
                case   258:
                  resword = &wordlist[82]; break;
                case   261:
                  resword = &wordlist[83]; break;
                case   262:
                  resword = &wordlist[84]; break;
                case   264:
                  resword = &wordlist[85]; break;
                case   265:
                  resword = &wordlist[86]; break;
                case   269:
                  resword = &wordlist[87]; break;
                case   271:
                  resword = &wordlist[88]; break;
                case   277:
                  resword = &wordlist[89]; break;
                case   280:
                  resword = &wordlist[90]; break;
                case   282:
                  resword = &wordlist[91]; break;
                case   286:
                  resword = &wordlist[92]; break;
                case   291:
                  resword = &wordlist[93]; break;
                case   293:
                  resword = &wordlist[94]; break;
                case   296:
                  resword = &wordlist[95]; break;
                case   298:
                  resword = &wordlist[96]; break;
                case   300:
                  resword = &wordlist[97]; break;
                case   301:
                  resword = &wordlist[98]; break;
                case   303:
                  resword = &wordlist[99]; break;
                case   304:
                  resword = &wordlist[100]; break;
                case   305:
                  resword = &wordlist[101]; break;
                case   307:
                  resword = &wordlist[102]; break;
                case   309:
                  resword = &wordlist[103]; break;
                case   314:
                  resword = &wordlist[104]; break;
                case   315:
                  resword = &wordlist[105]; break;
                case   324:
                  resword = &wordlist[106]; break;
                case   329:
                  resword = &wordlist[107]; break;
                case   332:
                  resword = &wordlist[108]; break;
                case   338:
                  resword = &wordlist[109]; break;
                case   339:
                  resword = &wordlist[110]; break;
                case   342:
                  resword = &wordlist[111]; break;
                case   343:
                  resword = &wordlist[112]; break;
                case   346:
                  resword = &wordlist[113]; break;
                case   349:
                  resword = &wordlist[114]; break;
                case   351:
                  resword = &wordlist[115]; break;
                case   352:
                  resword = &wordlist[116]; break;
                case   356:
                  resword = &wordlist[117]; break;
                case   357:
                  resword = &wordlist[118]; break;
                case   361:
                  resword = &wordlist[119]; break;
                case   363:
                  resword = &wordlist[120]; break;
                case   364:
                  resword = &wordlist[121]; break;
                case   365:
                  resword = &wordlist[122]; break;
                case   366:
                  resword = &wordlist[123]; break;
                case   367:
                  resword = &wordlist[124]; break;
                case   373:
                  resword = &wordlist[125]; break;
                case   387:
                  resword = &wordlist[126]; break;
                case   396:
                  resword = &wordlist[127]; break;
                case   409:
                  resword = &wordlist[128]; break;
                case   411:
                  resword = &wordlist[129]; break;
                case   415:
                  resword = &wordlist[130]; break;
                case   417:
                  resword = &wordlist[131]; break;
                case   418:
                  resword = &wordlist[132]; break;
                case   422:
                  resword = &wordlist[133]; break;
                case   423:
                  resword = &wordlist[134]; break;
                case   429:
                  resword = &wordlist[135]; break;
                case   430:
                  resword = &wordlist[136]; break;
                case   433:
                  resword = &wordlist[137]; break;
                case   434:
                  resword = &wordlist[138]; break;
                case   435:
                  resword = &wordlist[139]; break;
                case   440:
                  resword = &wordlist[140]; break;
                case   443:
                  resword = &wordlist[141]; break;
                case   445:
                  resword = &wordlist[142]; break;
                case   446:
                  resword = &wordlist[143]; break;
                case   448:
                  resword = &wordlist[144]; break;
                case   451:
                  resword = &wordlist[145];
                  if (*str == *resword->name && !strcmp (str + 1, resword->name + 1)) return resword;
                  resword = &wordlist[146];
                  if (*str == *resword->name && !strcmp (str + 1, resword->name + 1)) return resword;
                  return 0;
                case   452:
                  resword = &wordlist[147]; break;
                case   460:
                  resword = &wordlist[148]; break;
                case   461:
                  resword = &wordlist[149]; break;
                case   462:
                  resword = &wordlist[150]; break;
                case   463:
                  resword = &wordlist[151]; break;
                case   466:
                  resword = &wordlist[152]; break;
                case   475:
                  resword = &wordlist[153]; break;
                case   483:
                  resword = &wordlist[154]; break;
                case   486:
                  resword = &wordlist[155]; break;
                case   487:
                  resword = &wordlist[156]; break;
                case   488:
                  resword = &wordlist[157]; break;
                case   489:
                  resword = &wordlist[158]; break;
                case   491:
                  resword = &wordlist[159]; break;
                case   494:
                  resword = &wordlist[160]; break;
                case   498:
                  resword = &wordlist[161]; break;
                case   499:
                  resword = &wordlist[162]; break;
                case   505:
                  resword = &wordlist[163]; break;
                case   512:
                  resword = &wordlist[164]; break;
                case   513:
                  resword = &wordlist[165]; break;
                case   521:
                  resword = &wordlist[166]; break;
                case   540:
                  resword = &wordlist[167]; break;
                case   543:
                  resword = &wordlist[168]; break;
                case   546:
                  resword = &wordlist[169]; break;
                case   547:
                  resword = &wordlist[170]; break;
                case   549:
                  resword = &wordlist[171]; break;
                case   551:
                  resword = &wordlist[172]; break;
                case   554:
                  resword = &wordlist[173]; break;
                case   557:
                  resword = &wordlist[174]; break;
                case   558:
                  resword = &wordlist[175]; break;
                case   568:
                  resword = &wordlist[176]; break;
                case   574:
                  resword = &wordlist[177]; break;
                case   576:
                  resword = &wordlist[178]; break;
                case   583:
                  resword = &wordlist[179]; break;
                case   587:
                  resword = &wordlist[180]; break;
                case   590:
                  resword = &wordlist[181]; break;
                case   591:
                  resword = &wordlist[182]; break;
                case   592:
                  resword = &wordlist[183]; break;
                case   596:
                  resword = &wordlist[184]; break;
                case   599:
                  resword = &wordlist[185]; break;
                case   600:
                  resword = &wordlist[186]; break;
                case   603:
                  resword = &wordlist[187]; break;
                case   604:
                  resword = &wordlist[188]; break;
                case   607:
                  resword = &wordlist[189]; break;
                case   617:
                  resword = &wordlist[190]; break;
                case   618:
                  resword = &wordlist[191]; break;
                case   622:
                  resword = &wordlist[192]; break;
                case   623:
                  resword = &wordlist[193]; break;
                case   624:
                  resword = &wordlist[194]; break;
                case   628:
                  resword = &wordlist[195]; break;
                case   636:
                  resword = &wordlist[196]; break;
                case   639:
                  resword = &wordlist[197]; break;
                case   641:
                  resword = &wordlist[198]; break;
                case   650:
                  resword = &wordlist[199]; break;
                case   652:
                  resword = &wordlist[200]; break;
                case   661:
                  resword = &wordlist[201]; break;
                case   664:
                  resword = &wordlist[202]; break;
                case   670:
                  resword = &wordlist[203]; break;
                case   672:
                  resword = &wordlist[204]; break;
                case   682:
                  resword = &wordlist[205]; break;
                case   685:
                  resword = &wordlist[206]; break;
                case   691:
                  resword = &wordlist[207]; break;
                case   697:
                  resword = &wordlist[208]; break;
                case   707:
                  resword = &wordlist[209]; break;
                case   709:
                  resword = &wordlist[210]; break;
                case   717:
                  resword = &wordlist[211]; break;
                case   720:
                  resword = &wordlist[212]; break;
                case   727:
                  resword = &wordlist[213]; break;
                case   730:
                  resword = &wordlist[214]; break;
                case   731:
                  resword = &wordlist[215]; break;
                case   745:
                  resword = &wordlist[216]; break;
                case   748:
                  resword = &wordlist[217]; break;
                case   750:
                  resword = &wordlist[218]; break;
                case   751:
                  resword = &wordlist[219]; break;
                case   756:
                  resword = &wordlist[220]; break;
                case   758:
                  resword = &wordlist[221]; break;
                case   762:
                  resword = &wordlist[222]; break;
                case   768:
                  resword = &wordlist[223]; break;
                case   771:
                  resword = &wordlist[224]; break;
                case   773:
                  resword = &wordlist[225]; break;
                case   775:
                  resword = &wordlist[226]; break;
                case   776:
                  resword = &wordlist[227]; break;
                case   777:
                  resword = &wordlist[228]; break;
                case   779:
                  resword = &wordlist[229]; break;
                case   791:
                  resword = &wordlist[230]; break;
                case   807:
                  resword = &wordlist[231]; break;
                case   814:
                  resword = &wordlist[232]; break;
                case   815:
                  resword = &wordlist[233]; break;
                case   830:
                  resword = &wordlist[234]; break;
                case   833:
                  resword = &wordlist[235]; break;
                case   834:
                  resword = &wordlist[236]; break;
                case   846:
                  resword = &wordlist[237]; break;
                case   849:
                  resword = &wordlist[238]; break;
                case   875:
                  resword = &wordlist[239]; break;
                case   909:
                  resword = &wordlist[240]; break;
                case   910:
                  resword = &wordlist[241]; break;
                case   912:
                  resword = &wordlist[242]; break;
                case   926:
                  resword = &wordlist[243]; break;
                case   931:
                  resword = &wordlist[244]; break;
                case   933:
                  resword = &wordlist[245]; break;
                case   944:
                  resword = &wordlist[246]; break;
                case   947:
                  resword = &wordlist[247]; break;
                case   982:
                  resword = &wordlist[248]; break;
                case   986:
                  resword = &wordlist[249]; break;
                case   989:
                  resword = &wordlist[250]; break;
                case  1004:
                  resword = &wordlist[251]; break;
                case  1007:
                  resword = &wordlist[252]; break;
                case  1018:
                  resword = &wordlist[253]; break;
                case  1019:
                  resword = &wordlist[254]; break;
                case  1023:
                  resword = &wordlist[255]; break;
                case  1035:
                  resword = &wordlist[256]; break;
                case  1036:
                  resword = &wordlist[257]; break;
                case  1039:
                  resword = &wordlist[258]; break;
                case  1068:
                  resword = &wordlist[259]; break;
                case  1077:
                  resword = &wordlist[260]; break;
                case  1082:
                  resword = &wordlist[261]; break;
                case  1086:
                  resword = &wordlist[262]; break;
                case  1104:
                  resword = &wordlist[263]; break;
                case  1105:
                  resword = &wordlist[264]; break;
                case  1109:
                  resword = &wordlist[265]; break;
                case  1138:
                  resword = &wordlist[266]; break;
                case  1152:
                  resword = &wordlist[267]; break;
                case  1162:
                  resword = &wordlist[268]; break;
                case  1165:
                  resword = &wordlist[269]; break;
                case  1167:
                  resword = &wordlist[270]; break;
                case  1168:
                  resword = &wordlist[271]; break;
                case  1182:
                  resword = &wordlist[272]; break;
                case  1194:
                  resword = &wordlist[273]; break;
                case  1207:
                  resword = &wordlist[274]; break;
                case  1216:
                  resword = &wordlist[275]; break;
                case  1217:
                  resword = &wordlist[276]; break;
                case  1227:
                  resword = &wordlist[277]; break;
                case  1242:
                  resword = &wordlist[278]; break;
                case  1271:
                  resword = &wordlist[279]; break;
                case  1274:
                  resword = &wordlist[280]; break;
                case  1283:
                  resword = &wordlist[281]; break;
                case  1301:
                  resword = &wordlist[282]; break;
                case  1302:
                  resword = &wordlist[283]; break;
                case  1321:
                  resword = &wordlist[284]; break;
                case  1324:
                  resword = &wordlist[285]; break;
                case  1339:
                  resword = &wordlist[286]; break;
                case  1342:
                  resword = &wordlist[287]; break;
                case  1345:
                  resword = &wordlist[288]; break;
                case  1372:
                  resword = &wordlist[289]; break;
                case  1422:
                  resword = &wordlist[290]; break;
                case  1448:
                  resword = &wordlist[291]; break;
                case  1546:
                  resword = &wordlist[292]; break;
                case  1606:
                  resword = &wordlist[293]; break;
                case  1654:
                  resword = &wordlist[294]; break;
                case  1765:
                  resword = &wordlist[295]; break;
                case  1808:
                  resword = &wordlist[296]; break;
                case  1875:
                  resword = &wordlist[297]; break;
                case  1877:
                  resword = &wordlist[298]; break;
                case  1936:
                  resword = &wordlist[299]; break;
                case  1989:
                  resword = &wordlist[300]; break;
                case  2153:
                  resword = &wordlist[301]; break;
                case  2224:
                  resword = &wordlist[302]; break;
                case  2383:
                  resword = &wordlist[303]; break;
                case  2448:
                  resword = &wordlist[304]; break;
                case  2491:
                  resword = &wordlist[305]; break;
                case  2821:
                  resword = &wordlist[306]; break;
                default: return 0;
                }
              if (*str == *resword->name && !strcmp (str + 1, resword->name + 1))
                return resword;
              return 0;
            }
         }
    }
  return 0;
}
