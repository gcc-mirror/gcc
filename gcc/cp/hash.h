/* C code produced by gperf version 2.5 (GNU C++ version) */
/* Command-line: gperf -p -j1 -g -o -t -N is_reserved_word -k1,4,7,$ /yorick/splunge/jason/g++/small/devo/gcc/cp/gxx.gperf  */
/* Command-line: gperf -p -j1 -g -o -t -N is_reserved_word -k1,4,$,7 gplus.gperf  */
struct resword { char *name; short token; enum rid rid;};

#define TOTAL_KEYWORDS 98
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 16
#define MIN_HASH_VALUE 4
#define MAX_HASH_VALUE 258
/* maximum key range = 255, duplicates = 0 */

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
     259, 259, 259, 259, 259, 259, 259, 259, 259, 259,
     259, 259, 259, 259, 259, 259, 259, 259, 259, 259,
     259, 259, 259, 259, 259, 259, 259, 259, 259, 259,
     259, 259, 259, 259, 259, 259, 259, 259, 259, 259,
     259, 259, 259, 259, 259, 259, 259, 259, 259, 259,
     259, 259, 259, 259, 259, 259, 259, 259, 259, 259,
     259, 259, 259, 259, 259, 259, 259, 259, 259, 259,
     259, 259, 259, 259, 259, 259, 259, 259, 259, 259,
     259, 259, 259, 259, 259, 259, 259, 259, 259, 259,
     259, 259, 259, 259, 259,   0, 259,  27,  17,  20,
      40,   0,  64,   6,  10,  89, 259,   2, 110,  44,
      13, 107,  40,  10,  18,  55,   1,   3,   5,  17,
       2,   4, 259, 259, 259, 259, 259, 259,
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
        break;
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
      {"true",  CXX_TRUE, NORID,},
      {"",}, {"",}, 
      {"try",  TRY, NORID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"not",  '!', NORID,},
      {"xor_eq",  ASSIGN, NORID,},
      {"extern",  SCSPEC, RID_EXTERN,},
      {"",}, {"",}, {"",}, 
      {"xor",  '^', NORID,},
      {"case",  CASE, NORID,},
      {"",}, {"",}, 
      {"using",  USING, NORID,},
      {"__extension__",  EXTENSION, NORID},
      {"not_eq",  EQCOMPARE, NORID,},
      {"",}, {"",}, 
      {"continue",  CONTINUE, NORID,},
      {"new",  NEW, NORID,},
      {"__inline",  SCSPEC, RID_INLINE},
      {"",}, 
      {"__inline__",  SCSPEC, RID_INLINE},
      {"",}, {"",}, {"",}, 
      {"return",  RETURN, NORID,},
      {"",}, {"",}, 
      {"and_eq",  ASSIGN, NORID,},
      {"",}, {"",}, 
      {"delete",  DELETE, NORID,},
      {"typeid",  TYPEID, NORID,},
      {"__wchar_t",  TYPESPEC, RID_WCHAR  /* Unique to ANSI C++ */,},
      {"namespace",  NAMESPACE, NORID,},
      {"template",  TEMPLATE, RID_TEMPLATE,},
      {"break",  BREAK, NORID,},
      {"private",  VISSPEC, RID_PRIVATE,},
      {"typename",  TYPENAME_KEYWORD, NORID,},
      {"",}, 
      {"catch",  CATCH, NORID,},
      {"",}, {"",}, {"",}, {"",}, 
      {"char",  TYPESPEC, RID_CHAR,},
      {"",}, 
      {"__asm__",  GCC_ASM_KEYWORD, NORID},
      {"double",  TYPESPEC, RID_DOUBLE,},
      {"",}, 
      {"struct",  AGGR, RID_RECORD,},
      {"reinterpret_cast",  REINTERPRET_CAST, NORID,},
      {"",}, 
      {"static_cast",  STATIC_CAST, NORID,},
      {"",}, 
      {"and",  ANDAND, NORID,},
      {"typeof",  TYPEOF, NORID,},
      {"switch",  SWITCH, NORID,},
      {"",}, 
      {"asm",  ASM_KEYWORD, NORID,},
      {"",}, 
      {"default",  DEFAULT, NORID,},
      {"",}, 
      {"mutable",  SCSPEC, RID_MUTABLE,},
      {"short",  TYPESPEC, RID_SHORT,},
      {"signature",  AGGR, RID_SIGNATURE	/* Extension */,},
      {"const",  TYPE_QUAL, RID_CONST,},
      {"static",  SCSPEC, RID_STATIC,},
      {"",}, {"",}, 
      {"for",  FOR, NORID,},
      {"",}, {"",}, {"",}, 
      {"void",  TYPESPEC, RID_VOID,},
      {"bitand",  '&', NORID,},
      {"protected",  VISSPEC, RID_PROTECTED,},
      {"enum",  ENUM, NORID,},
      {"int",  TYPESPEC, RID_INT,},
      {"",}, {"",}, {"",}, 
      {"float",  TYPESPEC, RID_FLOAT,},
      {"",}, 
      {"__signed__",  TYPESPEC, RID_SIGNED},
      {"dynamic_cast",  DYNAMIC_CAST, NORID,},
      {"__attribute",  ATTRIBUTE, NORID},
      {"",}, 
      {"__attribute__",  ATTRIBUTE, NORID},
      {"__asm",  GCC_ASM_KEYWORD, NORID},
      {"",}, 
      {"const_cast",  CONST_CAST, NORID,},
      {"",}, {"",}, {"",}, 
      {"friend",  SCSPEC, RID_FRIEND,},
      {"",}, {"",}, {"",}, 
      {"signed",  TYPESPEC, RID_SIGNED,},
      {"this",  THIS, NORID,},
      {"__const",  TYPE_QUAL, RID_CONST},
      {"__const__",  TYPE_QUAL, RID_CONST},
      {"__volatile",  TYPE_QUAL, RID_VOLATILE},
      {"__null",  CONSTANT, RID_NULL},
      {"__volatile__",  TYPE_QUAL, RID_VOLATILE},
      {"__typeof__",  TYPEOF, NORID},
      {"or_eq",  ASSIGN, NORID,},
      {"",}, 
      {"false",  CXX_FALSE, NORID,},
      {"sizeof",  SIZEOF, NORID,},
      {"long",  TYPESPEC, RID_LONG,},
      {"or",  OROR, NORID,},
      {"union",  AGGR, RID_UNION,},
      {"__signature__",  AGGR, RID_SIGNATURE	/* Extension */,},
      {"throw",  THROW, NORID,},
      {"",}, 
      {"while",  WHILE, NORID,},
      {"register",  SCSPEC, RID_REGISTER,},
      {"__alignof__",  ALIGNOF, NORID},
      {"class",  AGGR, RID_CLASS,},
      {"typedef",  SCSPEC, RID_TYPEDEF,},
      {"__signed",  TYPESPEC, RID_SIGNED},
      {"",}, {"",}, 
      {"unsigned",  TYPESPEC, RID_UNSIGNED,},
      {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"__label__",  LABEL, NORID},
      {"bitor",  '|', NORID,},
      {"",}, 
      {"do",  DO, NORID,},
      {"volatile",  TYPE_QUAL, RID_VOLATILE,},
      {"",}, {"",}, {"",}, {"",}, 
      {"if",  IF, NORID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"__sigof__",  SIGOF, NORID		/* Extension */,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, {"",}, 
      {"compl",  '~', NORID,},
      {"public",  VISSPEC, RID_PUBLIC,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"__typeof",  TYPEOF, NORID},
      {"inline",  SCSPEC, RID_INLINE,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, 
      {"__alignof",  ALIGNOF, NORID},
      {"",}, {"",}, {"",}, 
      {"overload",  OVERLOAD, NORID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"explicit",  SCSPEC, RID_EXPLICIT,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"goto",  GOTO, NORID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"sigof",  SIGOF, NORID		/* Extension */,},
      {"",}, 
      {"virtual",  SCSPEC, RID_VIRTUAL,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"bool",  TYPESPEC, RID_BOOL,},
      {"",}, {"",}, {"",}, 
      {"auto",  SCSPEC, RID_AUTO,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, {"",}, 
      {"operator",  OPERATOR, NORID,},
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
