/* C code produced by gperf version 2.5 (GNU C++ version) */
/* Command-line: gperf -p -j1 -g -o -t -N is_reserved_word -k1,4,7,$ ../../../devo/gcc/cp/gxx.gperf  */
/* Command-line: gperf -p -j1 -g -o -t -N is_reserved_word -k1,4,$,7 gplus.gperf  */
struct resword { char *name; short token; enum rid rid;};

#define TOTAL_KEYWORDS 86
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 13
#define MIN_HASH_VALUE 4
#define MAX_HASH_VALUE 196
/* maximum key range = 193, duplicates = 0 */

#ifdef __GNUC__
inline
#endif
static unsigned int
hash (str, len)
     register char *str;
     register int unsigned len;
{
  static unsigned char asso_values[] =
    {
     197, 197, 197, 197, 197, 197, 197, 197, 197, 197,
     197, 197, 197, 197, 197, 197, 197, 197, 197, 197,
     197, 197, 197, 197, 197, 197, 197, 197, 197, 197,
     197, 197, 197, 197, 197, 197, 197, 197, 197, 197,
     197, 197, 197, 197, 197, 197, 197, 197, 197, 197,
     197, 197, 197, 197, 197, 197, 197, 197, 197, 197,
     197, 197, 197, 197, 197, 197, 197, 197, 197, 197,
     197, 197, 197, 197, 197, 197, 197, 197, 197, 197,
     197, 197, 197, 197, 197, 197, 197, 197, 197, 197,
     197, 197, 197, 197, 197,   0, 197,  93,   3,  35,
       3,   0,  71,   8,   4,  78, 197,   3,  30,   6,
      29,  18,  37, 197,  55,   0,   4,  11,   7,  20,
       0,   8, 197, 197, 197, 197, 197, 197,
    };
  register int hval = len;

  switch (hval)
    {
      default:
      case 7:
        hval += asso_values[str[6]];
      case 6:
      case 5:
      case 4:
        hval += asso_values[str[3]];
      case 3:
      case 2:
      case 1:
        hval += asso_values[str[0]];
    }
  return hval + asso_values[str[len - 1]];
}

#ifdef __GNUC__
inline
#endif
struct resword *
is_reserved_word (str, len)
     register char *str;
     register unsigned int len;
{
  static struct resword wordlist[] =
    {
      {"",}, {"",}, {"",}, {"",}, 
      {"else",  ELSE, NORID,},
      {"",}, {"",}, 
      {"__asm__",  GCC_ASM_KEYWORD, NORID},
      {"this",  THIS, NORID,},
      {"delete",  DELETE, NORID,},
      {"except",  EXCEPT, NORID		/* Extension */,},
      {"__asm",  GCC_ASM_KEYWORD, NORID},
      {"double",  TYPESPEC, RID_DOUBLE,},
      {"typeid",  TYPEID, NORID,},
      {"switch",  SWITCH, NORID,},
      {"try",  TRY, NORID			/* Extension */,},
      {"enum",  ENUM, NORID,},
      {"void",  TYPESPEC, RID_VOID,},
      {"",}, {"",}, {"",}, 
      {"struct",  AGGR, RID_RECORD,},
      {"",}, 
      {"do",  DO, NORID,},
      {"",}, {"",}, {"",}, {"",}, 
      {"__headof__",  HEADOF, NORID},
      {"",}, {"",}, 
      {"__const__",  TYPE_QUAL, RID_CONST},
      {"__volatile",  TYPE_QUAL, RID_VOLATILE},
      {"__const",  TYPE_QUAL, RID_CONST},
      {"__volatile__",  TYPE_QUAL, RID_VOLATILE},
      {"extern",  SCSPEC, RID_EXTERN,},
      {"__typeof__",  TYPEOF, NORID},
      {"",}, 
      {"signed",  TYPESPEC, RID_SIGNED,},
      {"case",  CASE, NORID,},
      {"class",  AGGR, RID_CLASS,},
      {"__classof__",  CLASSOF, NORID},
      {"__extension__",  EXTENSION, NORID},
      {"",}, 
      {"const",  TYPE_QUAL, RID_CONST,},
      {"static",  SCSPEC, RID_STATIC,},
      {"",}, 
      {"throw",  THROW, NORID		/* Extension */,},
      {"goto",  GOTO, NORID,},
      {"signature",  AGGR, RID_SIGNATURE	/* Extension */,},
      {"long",  TYPESPEC, RID_LONG,},
      {"private",  VISSPEC, RID_PRIVATE,},
      {"new",  NEW, NORID,},
      {"template",  TEMPLATE, NORID,},
      {"",}, 
      {"while",  WHILE, NORID,},
      {"",}, 
      {"protected",  VISSPEC, RID_PROTECTED,},
      {"continue",  CONTINUE, NORID,},
      {"",}, 
      {"raise",  RAISE, NORID		/* Extension */,},
      {"raises",  RAISES, NORID		/* Extension */,},
      {"",}, 
      {"union",  AGGR, RID_UNION,},
      {"short",  TYPESPEC, RID_SHORT,},
      {"",}, 
      {"__inline",  SCSPEC, RID_INLINE},
      {"",}, 
      {"__inline__",  SCSPEC, RID_INLINE},
      {"",}, 
      {"__alignof__",  ALIGNOF, NORID},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"sizeof",  SIZEOF, NORID,},
      {"virtual",  SCSPEC, RID_VIRTUAL,},
      {"catch",  CATCH, NORID,},
      {"friend",  SCSPEC, RID_FRIEND,},
      {"typeof",  TYPEOF, NORID,},
      {"",}, {"",}, 
      {"headof",  HEADOF, NORID,},
      {"int",  TYPESPEC, RID_INT,},
      {"",}, {"",}, 
      {"__signed__",  TYPESPEC, RID_SIGNED},
      {"__signed",  TYPESPEC, RID_SIGNED},
      {"",}, {"",}, {"",}, 
      {"__attribute",  ATTRIBUTE, NORID},
      {"sigof",  SIGOF, NORID		/* Extension */,},
      {"__attribute__",  ATTRIBUTE, NORID},
      {"",}, 
      {"__headof",  HEADOF, NORID},
      {"",}, {"",}, 
      {"unsigned",  TYPESPEC, RID_UNSIGNED,},
      {"return",  RETURN, NORID,},
      {"asm",  ASM_KEYWORD, NORID,},
      {"__wchar_t",  TYPESPEC, RID_WCHAR  /* Unique to ANSI C++ */,},
      {"break",  BREAK, NORID,},
      {"__typeof",  TYPEOF, NORID},
      {"mutable",  SCSPEC, RID_MUTABLE,},
      {"",}, 
      {"public",  VISSPEC, RID_PUBLIC,},
      {"",}, 
      {"__classof",  CLASSOF, NORID},
      {"default",  DEFAULT, NORID,},
      {"",}, {"",}, {"",}, {"",}, 
      {"exception",  AGGR, RID_EXCEPTION	/* Extension */,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"all",  ALL, NORID			/* Extension */,},
      {"",}, {"",}, 
      {"for",  FOR, NORID,},
      {"",}, {"",}, 
      {"__label__",  LABEL, NORID},
      {"auto",  SCSPEC, RID_AUTO,},
      {"",}, {"",}, {"",}, {"",}, 
      {"volatile",  TYPE_QUAL, RID_VOLATILE,},
      {"__alignof",  ALIGNOF, NORID},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"dynamic_cast",  DYNAMIC_CAST, NORID,},
      {"",}, 
      {"char",  TYPESPEC, RID_CHAR,},
      {"",}, 
      {"if",  IF, NORID,},
      {"",}, 
      {"typedef",  SCSPEC, RID_TYPEDEF,},
      {"operator",  OPERATOR, NORID,},
      {"reraise",  RERAISE, NORID		/* Extension */,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"inline",  SCSPEC, RID_INLINE,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, 
      {"float",  TYPESPEC, RID_FLOAT,},
      {"",}, {"",}, {"",}, 
      {"overload",  OVERLOAD, NORID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"classof",  CLASSOF, NORID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, 
      {"register",  SCSPEC, RID_REGISTER,},
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register char *s = wordlist[key].name;

          if (*s == *str && !strcmp (str + 1, s + 1))
            return &wordlist[key];
        }
    }
  return 0;
}
