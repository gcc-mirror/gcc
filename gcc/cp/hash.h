/* C code produced by gperf version 2.5 (GNU C++ version) */
/* Command-line: gperf -p -j1 -g -o -t -N is_reserved_word -k1,4,7,$ ../../../devo/gcc/cp/gxx.gperf  */
/* Command-line: gperf -p -j1 -g -o -t -N is_reserved_word -k1,4,$,7 gplus.gperf  */
struct resword { char *name; short token; enum rid rid;};

#define TOTAL_KEYWORDS 80
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 13
#define MIN_HASH_VALUE 4
#define MAX_HASH_VALUE 166
/* maximum key range = 163, duplicates = 0 */

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
     167, 167, 167, 167, 167, 167, 167, 167, 167, 167,
     167, 167, 167, 167, 167, 167, 167, 167, 167, 167,
     167, 167, 167, 167, 167, 167, 167, 167, 167, 167,
     167, 167, 167, 167, 167, 167, 167, 167, 167, 167,
     167, 167, 167, 167, 167, 167, 167, 167, 167, 167,
     167, 167, 167, 167, 167, 167, 167, 167, 167, 167,
     167, 167, 167, 167, 167, 167, 167, 167, 167, 167,
     167, 167, 167, 167, 167, 167, 167, 167, 167, 167,
     167, 167, 167, 167, 167, 167, 167, 167, 167, 167,
     167, 167, 167, 167, 167,   0, 167,  36,   6,  60,
      17,   0,  16,   5,  41,  38, 167,  11,  22,   7,
      26,   0,   4, 167,  22,   0,   4,  44,  19,   8,
       5,  18, 167, 167, 167, 167, 167, 167,
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
      {"goto",  GOTO, NORID,},
      {"__headof__",  HEADOF, NORID},
      {"",}, 
      {"__asm",  GCC_ASM_KEYWORD, NORID},
      {"__const__",  TYPE_QUAL, RID_CONST},
      {"__volatile",  TYPE_QUAL, RID_VOLATILE},
      {"__const",  TYPE_QUAL, RID_CONST},
      {"__volatile__",  TYPE_QUAL, RID_VOLATILE},
      {"throw",  THROW, NORID,},
      {"enum",  ENUM, NORID,},
      {"do",  DO, NORID,},
      {"template",  TEMPLATE, NORID,},
      {"sigof",  SIGOF, NORID		/* Extension */,},
      {"sizeof",  SIZEOF, NORID,},
      {"delete",  DELETE, NORID,},
      {"__headof",  HEADOF, NORID},
      {"try",  TRY, NORID,},
      {"typeof",  TYPEOF, NORID,},
      {"typeid",  TYPEID, NORID,},
      {"__typeof__",  TYPEOF, NORID},
      {"double",  TYPESPEC, RID_DOUBLE,},
      {"private",  VISSPEC, RID_PRIVATE,},
      {"short",  TYPESPEC, RID_SHORT,},
      {"extern",  SCSPEC, RID_EXTERN,},
      {"__classof__",  CLASSOF, NORID},
      {"",}, 
      {"while",  WHILE, NORID,},
      {"long",  TYPESPEC, RID_LONG,},
      {"new",  NEW, NORID,},
      {"protected",  VISSPEC, RID_PROTECTED,},
      {"friend",  SCSPEC, RID_FRIEND,},
      {"auto",  SCSPEC, RID_AUTO,},
      {"for",  FOR, NORID,},
      {"__typeof",  TYPEOF, NORID},
      {"typedef",  SCSPEC, RID_TYPEDEF,},
      {"__extension__",  EXTENSION, NORID},
      {"int",  TYPESPEC, RID_INT,},
      {"asm",  ASM_KEYWORD, NORID,},
      {"__classof",  CLASSOF, NORID},
      {"__signed__",  TYPESPEC, RID_SIGNED},
      {"signed",  TYPESPEC, RID_SIGNED,},
      {"mutable",  SCSPEC, RID_MUTABLE,},
      {"switch",  SWITCH, NORID,},
      {"operator",  OPERATOR, NORID,},
      {"__attribute",  ATTRIBUTE, NORID},
      {"struct",  AGGR, RID_RECORD,},
      {"__attribute__",  ATTRIBUTE, NORID},
      {"if",  IF, NORID,},
      {"void",  TYPESPEC, RID_VOID,},
      {"break",  BREAK, NORID,},
      {"__alignof__",  ALIGNOF, NORID},
      {"__inline",  SCSPEC, RID_INLINE},
      {"float",  TYPESPEC, RID_FLOAT,},
      {"__inline__",  SCSPEC, RID_INLINE},
      {"__signed",  TYPESPEC, RID_SIGNED},
      {"case",  CASE, NORID,},
      {"class",  AGGR, RID_CLASS,},
      {"",}, 
      {"__label__",  LABEL, NORID},
      {"default",  DEFAULT, NORID,},
      {"const",  TYPE_QUAL, RID_CONST,},
      {"static",  SCSPEC, RID_STATIC,},
      {"",}, {"",}, 
      {"__alignof",  ALIGNOF, NORID},
      {"virtual",  SCSPEC, RID_VIRTUAL,},
      {"union",  AGGR, RID_UNION,},
      {"",}, {"",}, {"",}, 
      {"signature",  AGGR, RID_SIGNATURE	/* Extension */,},
      {"headof",  HEADOF, NORID,},
      {"",}, 
      {"inline",  SCSPEC, RID_INLINE,},
      {"overload",  OVERLOAD, NORID,},
      {"",}, 
      {"volatile",  TYPE_QUAL, RID_VOLATILE,},
      {"",}, {"",}, {"",}, {"",}, 
      {"register",  SCSPEC, RID_REGISTER,},
      {"",}, 
      {"public",  VISSPEC, RID_PUBLIC,},
      {"",}, {"",}, 
      {"__wchar_t",  TYPESPEC, RID_WCHAR  /* Unique to ANSI C++ */,},
      {"",}, {"",}, 
      {"return",  RETURN, NORID,},
      {"classof",  CLASSOF, NORID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"unsigned",  TYPESPEC, RID_UNSIGNED,},
      {"char",  TYPESPEC, RID_CHAR,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"continue",  CONTINUE, NORID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, {"",}, 
      {"dynamic_cast",  DYNAMIC_CAST, NORID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      
      {"catch",  CATCH, NORID,},
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
