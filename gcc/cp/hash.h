/* C code produced by gperf version 2.5 (GNU C++ version) */
/* Command-line: gperf -p -j1 -g -o -t -N is_reserved_word -k1,4,7,$ ../../../devo/gcc/cp/gxx.gperf  */
/* Command-line: gperf -p -j1 -g -o -t -N is_reserved_word -k1,4,$,7 gplus.gperf  */
struct resword { char *name; short token; enum rid rid;};

#define TOTAL_KEYWORDS 101
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 16
#define MIN_HASH_VALUE 4
#define MAX_HASH_VALUE 210
/* maximum key range = 207, duplicates = 0 */

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
     211, 211, 211, 211, 211, 211, 211, 211, 211, 211,
     211, 211, 211, 211, 211, 211, 211, 211, 211, 211,
     211, 211, 211, 211, 211, 211, 211, 211, 211, 211,
     211, 211, 211, 211, 211, 211, 211, 211, 211, 211,
     211, 211, 211, 211, 211, 211, 211, 211, 211, 211,
     211, 211, 211, 211, 211, 211, 211, 211, 211, 211,
     211, 211, 211, 211, 211, 211, 211, 211, 211, 211,
     211, 211, 211, 211, 211, 211, 211, 211, 211, 211,
     211, 211, 211, 211, 211, 211, 211, 211, 211, 211,
     211, 211, 211, 211, 211,   0, 211,  35,   1,  69,
      61,   0,  19,  65,  20, 100, 211,   5,  11,  52,
       3,  25,   6,   2,  31,  26,   4,  41,  24,  64,
      10,  24, 211, 211, 211, 211, 211, 211,
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
      {"",}, {"",}, {"",}, 
      {"true",  CXX_TRUE, NORID,},
      {"extern",  SCSPEC, RID_EXTERN,},
      {"not",  '!', NORID,},
      {"not_eq",  EQCOMPARE, NORID,},
      {"",}, {"",}, 
      {"__inline",  SCSPEC, RID_INLINE},
      {"",}, 
      {"__inline__",  SCSPEC, RID_INLINE},
      {"",}, 
      {"xor_eq",  ASSIGN, NORID,},
      {"",}, {"",}, {"",}, 
      {"template",  TEMPLATE, RID_TEMPLATE,},
      {"",}, {"",}, 
      {"__alignof__",  ALIGNOF, NORID},
      {"__extension__",  EXTENSION, NORID},
      {"bool",  TYPESPEC, RID_BOOL,},
      {"",}, 
      {"typeof",  TYPEOF, NORID,},
      {"",}, 
      {"try",  TRY, NORID,},
      {"or_eq",  ASSIGN, NORID,},
      {"__asm__",  GCC_ASM_KEYWORD, NORID},
      {"",}, 
      {"__headof__",  HEADOF, NORID},
      {"",}, 
      {"private",  VISSPEC, RID_PRIVATE,},
      {"__const__",  TYPE_QUAL, RID_CONST},
      {"__volatile",  TYPE_QUAL, RID_VOLATILE},
      {"__const",  TYPE_QUAL, RID_CONST},
      {"__volatile__",  TYPE_QUAL, RID_VOLATILE},
      {"__alignof",  ALIGNOF, NORID},
      {"and_eq",  ASSIGN, NORID,},
      {"xor",  '^', NORID,},
      {"static_cast",  STATIC_CAST, NORID,},
      {"break",  BREAK, NORID,},
      {"namespace",  NAMESPACE, NORID,},
      {"__classof__",  CLASSOF, NORID},
      {"typedef",  SCSPEC, RID_TYPEDEF,},
      {"false",  CXX_FALSE, NORID,},
      {"sizeof",  SIZEOF, NORID,},
      {"__headof",  HEADOF, NORID},
      {"for",  FOR, NORID,},
      {"",}, 
      {"__label__",  LABEL, NORID},
      {"switch",  SWITCH, NORID,},
      {"virtual",  SCSPEC, RID_VIRTUAL,},
      {"or",  OROR, NORID,},
      {"__typeof__",  TYPEOF, NORID},
      {"this",  THIS, NORID,},
      {"",}, 
      {"bitor",  '|', NORID,},
      {"float",  TYPESPEC, RID_FLOAT,},
      {"typename",  TYPENAME_KEYWORD, NORID,},
      {"__classof",  CLASSOF, NORID},
      {"short",  TYPESPEC, RID_SHORT,},
      {"delete",  DELETE, NORID,},
      {"double",  TYPESPEC, RID_DOUBLE,},
      {"",}, 
      {"new",  NEW, NORID,},
      {"typeid",  TYPEID, NORID,},
      {"",}, 
      {"case",  CASE, NORID,},
      {"union",  AGGR, RID_UNION,},
      {"sigof",  SIGOF, NORID		/* Extension */,},
      {"__typeof",  TYPEOF, NORID},
      {"struct",  AGGR, RID_RECORD,},
      {"volatile",  TYPE_QUAL, RID_VOLATILE,},
      {"signature",  AGGR, RID_SIGNATURE	/* Extension */,},
      {"while",  WHILE, NORID,},
      {"return",  RETURN, NORID,},
      {"",}, 
      {"__asm",  GCC_ASM_KEYWORD, NORID},
      {"protected",  VISSPEC, RID_PROTECTED,},
      {"reinterpret_cast",  REINTERPRET_CAST, NORID,},
      {"friend",  SCSPEC, RID_FRIEND,},
      {"",}, 
      {"do",  DO, NORID,},
      {"auto",  SCSPEC, RID_AUTO,},
      {"asm",  ASM_KEYWORD, NORID,},
      {"compl",  '~', NORID,},
      {"public",  VISSPEC, RID_PUBLIC,},
      {"",}, 
      {"mutable",  SCSPEC, RID_MUTABLE,},
      {"",}, 
      {"signed",  TYPESPEC, RID_SIGNED,},
      {"",}, 
      {"throw",  THROW, NORID,},
      {"and",  ANDAND, NORID,},
      {"",}, {"",}, {"",}, 
      {"bitand",  '&', NORID,},
      {"const",  TYPE_QUAL, RID_CONST,},
      {"static",  SCSPEC, RID_STATIC,},
      {"headof",  HEADOF, NORID,},
      {"int",  TYPESPEC, RID_INT,},
      {"enum",  ENUM, NORID,},
      {"",}, 
      {"__signed__",  TYPESPEC, RID_SIGNED},
      {"default",  DEFAULT, NORID,},
      {"",}, 
      {"__wchar_t",  TYPESPEC, RID_WCHAR  /* Unique to ANSI C++ */,},
      {"using",  USING, NORID,},
      {"__attribute",  ATTRIBUTE, NORID},
      {"",}, 
      {"__attribute__",  ATTRIBUTE, NORID},
      {"",}, 
      {"goto",  GOTO, NORID,},
      {"operator",  OPERATOR, NORID,},
      {"if",  IF, NORID,},
      {"continue",  CONTINUE, NORID,},
      {"explicit",  SCSPEC, RID_EXPLICIT,},
      {"",}, {"",}, 
      {"class",  AGGR, RID_CLASS,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"char",  TYPESPEC, RID_CHAR,},
      {"",}, {"",}, {"",}, {"",}, 
      {"classof",  CLASSOF, NORID,},
      {"",}, {"",}, {"",}, {"",}, 
      {"long",  TYPESPEC, RID_LONG,},
      {"",}, {"",}, {"",}, {"",}, 
      {"void",  TYPESPEC, RID_VOID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"overload",  OVERLOAD, NORID,},
      {"",}, {"",}, 
      {"catch",  CATCH, NORID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"__signed",  TYPESPEC, RID_SIGNED},
      {"register",  SCSPEC, RID_REGISTER,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"const_cast",  CONST_CAST, NORID,},
      {"",}, {"",}, 
      {"dynamic_cast",  DYNAMIC_CAST, NORID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"inline",  SCSPEC, RID_INLINE,},
      {"",}, {"",}, {"",}, 
      {"unsigned",  TYPESPEC, RID_UNSIGNED,},
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
