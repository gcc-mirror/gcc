/* C code produced by gperf version 2.5 (GNU C++ version) */
/* Command-line: gperf -p -j1 -g -o -t -N is_reserved_word -k1,4,7,$ /deneb/blob/jason/g++/small/devo/gcc/cp/gxx.gperf  */
/* Command-line: gperf -p -j1 -g -o -t -N is_reserved_word -k1,4,$,7 gplus.gperf  */
struct resword { char *name; short token; enum rid rid;};

#define TOTAL_KEYWORDS 86
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 16
#define MIN_HASH_VALUE 4
#define MAX_HASH_VALUE 171
/* maximum key range = 168, duplicates = 0 */

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
     172, 172, 172, 172, 172, 172, 172, 172, 172, 172,
     172, 172, 172, 172, 172, 172, 172, 172, 172, 172,
     172, 172, 172, 172, 172, 172, 172, 172, 172, 172,
     172, 172, 172, 172, 172, 172, 172, 172, 172, 172,
     172, 172, 172, 172, 172, 172, 172, 172, 172, 172,
     172, 172, 172, 172, 172, 172, 172, 172, 172, 172,
     172, 172, 172, 172, 172, 172, 172, 172, 172, 172,
     172, 172, 172, 172, 172, 172, 172, 172, 172, 172,
     172, 172, 172, 172, 172, 172, 172, 172, 172, 172,
     172, 172, 172, 172, 172,   0, 172,  36,   1,  61,
       0,   0,  30,  44,  44,  35, 172,   7,  12,  53,
      40,  17,   6, 172,  28,   2,   4,  35,  31,  51,
       5,   7, 172, 172, 172, 172, 172, 172,
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
      {"",}, 
      {"delete",  DELETE, NORID,},
      {"double",  TYPESPEC, RID_DOUBLE,},
      {"true",  CXX_TRUE, NORID,},
      {"__asm__",  GCC_ASM_KEYWORD, NORID},
      {"typeid",  TYPEID, NORID,},
      {"",}, 
      {"this",  THIS, NORID,},
      {"",}, 
      {"try",  TRY, NORID,},
      {"",}, {"",}, {"",}, {"",}, 
      {"do",  DO, NORID,},
      {"",}, 
      {"static_cast",  STATIC_CAST, NORID,},
      {"template",  TEMPLATE, RID_TEMPLATE,},
      {"protected",  VISSPEC, RID_PROTECTED,},
      {"",}, 
      {"__classof__",  CLASSOF, NORID},
      {"",}, 
      {"__headof__",  HEADOF, NORID},
      {"",}, 
      {"bool",  TYPESPEC, RID_BOOL,},
      {"__const__",  TYPE_QUAL, RID_CONST},
      {"__volatile",  TYPE_QUAL, RID_VOLATILE},
      {"__const",  TYPE_QUAL, RID_CONST},
      {"__volatile__",  TYPE_QUAL, RID_VOLATILE},
      {"__typeof__",  TYPEOF, NORID},
      {"void",  TYPESPEC, RID_VOID,},
      {"friend",  SCSPEC, RID_FRIEND,},
      {"false",  CXX_FALSE, NORID,},
      {"sizeof",  SIZEOF, NORID,},
      {"short",  TYPESPEC, RID_SHORT,},
      {"typeof",  TYPEOF, NORID,},
      {"",}, 
      {"int",  TYPESPEC, RID_INT,},
      {"__signed",  TYPESPEC, RID_SIGNED},
      {"private",  VISSPEC, RID_PRIVATE,},
      {"__signed__",  TYPESPEC, RID_SIGNED},
      {"extern",  SCSPEC, RID_EXTERN,},
      {"struct",  AGGR, RID_RECORD,},
      {"signed",  TYPESPEC, RID_SIGNED,},
      {"break",  BREAK, NORID,},
      {"__attribute",  ATTRIBUTE, NORID},
      {"default",  DEFAULT, NORID,},
      {"__attribute__",  ATTRIBUTE, NORID},
      {"__classof",  CLASSOF, NORID},
      {"sigof",  SIGOF, NORID		/* Extension */,},
      {"__headof",  HEADOF, NORID},
      {"switch",  SWITCH, NORID,},
      {"__label__",  LABEL, NORID},
      {"__extension__",  EXTENSION, NORID},
      {"",}, 
      {"__asm",  GCC_ASM_KEYWORD, NORID},
      {"for",  FOR, NORID,},
      {"__typeof",  TYPEOF, NORID},
      {"__alignof__",  ALIGNOF, NORID},
      {"",}, 
      {"case",  CASE, NORID,},
      {"virtual",  SCSPEC, RID_VIRTUAL,},
      {"if",  IF, NORID,},
      {"while",  WHILE, NORID,},
      {"",}, 
      {"class",  AGGR, RID_CLASS,},
      {"typedef",  SCSPEC, RID_TYPEDEF,},
      {"const",  TYPE_QUAL, RID_CONST,},
      {"static",  SCSPEC, RID_STATIC,},
      {"auto",  SCSPEC, RID_AUTO,},
      {"float",  TYPESPEC, RID_FLOAT,},
      {"inline",  SCSPEC, RID_INLINE,},
      {"throw",  THROW, NORID,},
      {"unsigned",  TYPESPEC, RID_UNSIGNED,},
      {"",}, 
      {"headof",  HEADOF, NORID,},
      {"",}, 
      {"goto",  GOTO, NORID,},
      {"",}, {"",}, 
      {"public",  VISSPEC, RID_PUBLIC,},
      {"signature",  AGGR, RID_SIGNATURE	/* Extension */,},
      {"volatile",  TYPE_QUAL, RID_VOLATILE,},
      {"__inline",  SCSPEC, RID_INLINE},
      {"overload",  OVERLOAD, NORID,},
      {"__inline__",  SCSPEC, RID_INLINE},
      {"__alignof",  ALIGNOF, NORID},
      {"asm",  ASM_KEYWORD, NORID,},
      {"",}, 
      {"new",  NEW, NORID,},
      {"",}, 
      {"mutable",  SCSPEC, RID_MUTABLE,},
      {"union",  AGGR, RID_UNION,},
      {"operator",  OPERATOR, NORID,},
      {"register",  SCSPEC, RID_REGISTER,},
      {"",}, {"",}, 
      {"__wchar_t",  TYPESPEC, RID_WCHAR  /* Unique to ANSI C++ */,},
      {"",}, 
      {"long",  TYPESPEC, RID_LONG,},
      {"",}, {"",}, {"",}, 
      {"continue",  CONTINUE, NORID,},
      {"return",  RETURN, NORID,},
      {"enum",  ENUM, NORID,},
      {"",}, {"",}, 
      {"dynamic_cast",  DYNAMIC_CAST, NORID,},
      {"",}, {"",}, 
      {"reinterpret_cast",  REINTERPRET_CAST, NORID,},
      {"",}, {"",}, {"",}, {"",}, 
      {"char",  TYPESPEC, RID_CHAR,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"classof",  CLASSOF, NORID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"const_cast",  CONST_CAST, NORID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, {"",}, {"",}, {"",}, 
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
