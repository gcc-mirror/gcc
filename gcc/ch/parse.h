typedef union {
  long itype;
  tree ttype;
  enum tree_code code;
  char *filename;
  int lineno;
} YYSTYPE;
extern YYSTYPE yylval;

/* DELAY is defined in the standard headers on some platforms like
   SunOS 4.1.4.  */
#ifdef DELAY
#undef DELAY
#endif

enum terminal
{
  /*EOF = 0,*/
  last_char_nonterminal = 256,
  /* Please keep these in alphabetic order, for easier reference and updating.
   */
  ABSOLUTE, ACCESS, AFTER, ALL, ALLOCATE, AND, ANDIF, ARRAY, 
  ARROW, ASGN, ASM_KEYWORD, ASSERT, ASSOCIATION, AT,
  BASED, BEGINTOKEN, BIN, BIT, BITSTRING, BODY, BOOLS, BUFFER,
  BUFFERNAME, BUFFER_CODE, BY,
  CALL, CASE, CAUSE, CDDEL, CHAR, CHARS, COLON, COMMA, CONCAT, CONST,
  CONTINUE, CYCLE,
  DCL, DELAY, DIV, DO, DOT, DOWN, DYNAMIC, 
  ELSE, ELSIF, END, ENTRY, EQL, ESAC, EVENT, EVENT_CODE, EVER,
  EXCEPTIONS, EXIT,
  EXPR, /* an expression that has been pushed back */
  FI, FLOATING, FOR, FORBID,
  GENERAL, GOTO, GRANT, GT, GTE,
  HEADEREL,
  IF, IGNORED_DIRECTIVE, IN, INIT, INOUT, INLINE,
  LC, LOC, LPC, LPRN, LT, LTE,
  MOD, MODULE, MUL, 
  NAME, NE, NEW, NEWMODE, NONREF, NOT, NUMBER,
  OD, OF, ON, OR, ORIF,
  PARAMATTR, PERVASIVE, PLUS, POWERSET,
  PREFIXED, PRIORITY, PROC, PROCESS,
  RANGE, RC, READ, READTEXT, RECEIVE, RECURSIVE, REF, REGION, REM,
  RESULT, RETURN, RETURNS, ROUND, ROW, RPC, RPRN, RPRN_COLON,
  SAME, SC, SEIZE, SEND, SET, SHARED, SIGNAL, SIGNALNAME, SIMPLE,
  SINGLECHAR, SPEC, START, STATIC, STEP, STOP, STREAM, STRING, 
  STRUCT, SUB, SYN, SYNMODE,
  TERMINATE, TEXT, THEN, THIS, TIMEOUT, TO, TRUNC, TYPENAME, 
  UP, USAGE,
  VARYING, 
  WHERE, WHILE, WITH,
  XOR,

/* These tokens only used within ch-lex.l to process compiler directives */
  ALL_STATIC_OFF, ALL_STATIC_ON, EMPTY_OFF, EMPTY_ON,
  GRANT_FILE_SIZE, PROCESS_TYPE_TOKEN, RANGE_OFF, RANGE_ON,
  SEND_BUFFER_DEFAULT_PRIORITY, SEND_SIGNAL_DEFAULT_PRIORITY,
  SIGNAL_CODE, SIGNAL_MAX_LENGTH, USE_SEIZE_FILE, USE_SEIZE_FILE_RESTRICTED,
  USE_GRANT_FILE, 

  /* These tokens are recognized, and reported as errors, by the lexer. */
  CONTEXT, REMOTE,

  /* These tokens are recognized in the lexer, and completely
     ignored. They represent unimplemented features in the
     current version of GNU CHILL. */
  NOPACK, PACK,

/* These tokens are recognized in the lexer, and returned
   as reserved tokens, to prevent users from using them
   accidently (they'll cause a parser syntax error).  They
   represent unimplemented features in the current version
   of GNU CHILL. */
  POS, /*STEP, ROW,*/

/* This token is passed back to the parser when an the main 
   input file (not a seize file) has  reached end-of-file. */
  END_PASS_1,

  EMPTY, UMINUS,

  dummy_last_terminal
};
