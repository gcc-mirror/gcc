/* C code produced by gperf version 2.5 (GNU C++ version) */
/* Command-line: gperf -p -j1 -g -o -t -N is_reserved_word -k1,4,$,7 gxx.gperf  */
struct resword { char *name; short token; enum rid rid;};

#define TOTAL_KEYWORDS 103
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 16
#define MIN_HASH_VALUE 4
#define MAX_HASH_VALUE 195
/* maximum key range = 192, duplicates = 0 */

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
     196, 196, 196, 196, 196, 196, 196, 196, 196, 196,
     196, 196, 196, 196, 196, 196, 196, 196, 196, 196,
     196, 196, 196, 196, 196, 196, 196, 196, 196, 196,
     196, 196, 196, 196, 196, 196, 196, 196, 196, 196,
     196, 196, 196, 196, 196, 196, 196, 196, 196, 196,
     196, 196, 196, 196, 196, 196, 196, 196, 196, 196,
     196, 196, 196, 196, 196, 196, 196, 196, 196, 196,
     196, 196, 196, 196, 196, 196, 196, 196, 196, 196,
     196, 196, 196, 196, 196, 196, 196, 196, 196, 196,
     196, 196, 196, 196, 196,   0, 196,  80,  26,  28,
      44,   0,  49,  38,   6,  81, 196,   2,   0,  41,
      16,  47,   4,  31,  32,   5,   6,  62,  20,  96,
      17,  25, 196, 196, 196, 196, 196, 196,
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
      {"",}, 
      {"__real",  REALPART, NORID},
      {"",}, 
      {"__real__",  REALPART, NORID},
      {"",}, 
      {"true",  CXX_TRUE, NORID,},
      {"",}, 
      {"__asm__",  GCC_ASM_KEYWORD, NORID},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"this",  THIS, NORID,},
      {"",}, 
      {"extern",  SCSPEC, RID_EXTERN,},
      {"switch",  SWITCH, NORID,},
      {"template",  TEMPLATE, RID_TEMPLATE,},
      {"not",  '!', NORID,},
      {"",}, 
      {"__alignof__",  ALIGNOF, NORID},
      {"static_cast",  STATIC_CAST, NORID,},
      {"",}, 
      {"bool",  TYPESPEC, RID_BOOL,},
      {"private",  VISSPEC, RID_PRIVATE,},
      {"case",  CASE, NORID,},
      {"virtual",  SCSPEC, RID_VIRTUAL,},
      {"try",  TRY, NORID,},
      {"",}, {"",}, 
      {"compl",  '~', NORID,},
      {"public",  VISSPEC, RID_PUBLIC,},
      {"",}, 
      {"__inline",  SCSPEC, RID_INLINE},
      {"",}, 
      {"__inline__",  SCSPEC, RID_INLINE},
      {"class",  AGGR, RID_CLASS,},
      {"const",  CV_QUALIFIER, RID_CONST,},
      {"static",  SCSPEC, RID_STATIC,},
      {"__extension__",  EXTENSION, NORID},
      {"",}, 
      {"short",  TYPESPEC, RID_SHORT,},
      {"__imag__",  IMAGPART, NORID},
      {"delete",  DELETE, NORID,},
      {"__asm",  GCC_ASM_KEYWORD, NORID},
      {"xor",  '^', NORID,},
      {"not_eq",  EQCOMPARE, NORID,},
      {"xor_eq",  ASSIGN, NORID,},
      {"typename",  TYPENAME_KEYWORD, NORID,},
      {"typeid",  TYPEID, NORID,},
      {"",}, 
      {"__complex__",  TYPESPEC, RID_COMPLEX},
      {"false",  CXX_FALSE, NORID,},
      {"sizeof",  SIZEOF, NORID,},
      {"typeof",  TYPEOF, NORID,},
      {"__const__",  CV_QUALIFIER, RID_CONST},
      {"__volatile",  CV_QUALIFIER, RID_VOLATILE},
      {"",}, 
      {"__volatile__",  CV_QUALIFIER, RID_VOLATILE},
      {"__const",  CV_QUALIFIER, RID_CONST},
      {"catch",  CATCH, NORID,},
      {"__null",  CONSTANT, RID_NULL},
      {"protected",  VISSPEC, RID_PROTECTED,},
      {"",}, 
      {"signed",  TYPESPEC, RID_SIGNED,},
      {"",}, 
      {"__complex",  TYPESPEC, RID_COMPLEX},
      {"__alignof",  ALIGNOF, NORID},
      {"__wchar_t",  TYPESPEC, RID_WCHAR  /* Unique to ANSI C++ */,},
      {"double",  TYPESPEC, RID_DOUBLE,},
      {"const_cast",  CONST_CAST, NORID,},
      {"",}, 
      {"struct",  AGGR, RID_RECORD,},
      {"long",  TYPESPEC, RID_LONG,},
      {"or",  OROR, NORID,},
      {"__typeof__",  TYPEOF, NORID},
      {"or_eq",  ASSIGN, NORID,},
      {"for",  FOR, NORID,},
      {"__imag",  IMAGPART, NORID},
      {"enum",  ENUM, NORID,},
      {"",}, {"",}, 
      {"__label__",  LABEL, NORID},
      {"int",  TYPESPEC, RID_INT,},
      {"__signed__",  TYPESPEC, RID_SIGNED},
      {"signature",  AGGR, RID_SIGNATURE	/* Extension */,},
      {"do",  DO, NORID,},
      {"",}, 
      {"explicit",  SCSPEC, RID_EXPLICIT,},
      {"char",  TYPESPEC, RID_CHAR,},
      {"",}, 
      {"__attribute",  ATTRIBUTE, NORID},
      {"friend",  SCSPEC, RID_FRIEND,},
      {"__attribute__",  ATTRIBUTE, NORID},
      {"while",  WHILE, NORID,},
      {"reinterpret_cast",  REINTERPRET_CAST, NORID,},
      {"",}, 
      {"continue",  CONTINUE, NORID,},
      {"namespace",  NAMESPACE, NORID,},
      {"sigof",  SIGOF, NORID		/* Extension */,},
      {"",}, 
      {"volatile",  CV_QUALIFIER, RID_VOLATILE,},
      {"",}, 
      {"bitor",  '|', NORID,},
      {"typedef",  SCSPEC, RID_TYPEDEF,},
      {"void",  TYPESPEC, RID_VOID,},
      {"break",  BREAK, NORID,},
      {"",}, 
      {"new",  NEW, NORID,},
      {"return",  RETURN, NORID,},
      {"and_eq",  ASSIGN, NORID,},
      {"",}, {"",}, {"",}, 
      {"using",  USING, NORID,},
      {"",}, {"",}, 
      {"asm",  ASM_KEYWORD, NORID,},
      {"",}, {"",}, 
      {"and",  ANDAND, NORID,},
      {"mutable",  SCSPEC, RID_MUTABLE,},
      {"__typeof",  TYPEOF, NORID},
      {"union",  AGGR, RID_UNION,},
      {"",}, 
      {"if",  IF, NORID,},
      {"__signed",  TYPESPEC, RID_SIGNED},
      {"",}, {"",}, 
      {"goto",  GOTO, NORID,},
      {"",}, {"",}, 
      {"__sigof__",  SIGOF, NORID		/* Extension */,},
      {"float",  TYPESPEC, RID_FLOAT,},
      {"",}, {"",}, 
      {"default",  DEFAULT, NORID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"register",  SCSPEC, RID_REGISTER,},
      {"throw",  THROW, NORID,},
      {"",}, 
      {"bitand",  '&', NORID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"operator",  OPERATOR, NORID,},
      {"",}, 
      {"inline",  SCSPEC, RID_INLINE,},
      {"",}, 
      {"dynamic_cast",  DYNAMIC_CAST, NORID,},
      {"",}, {"",}, {"",}, 
      {"__signature__",  AGGR, RID_SIGNATURE	/* Extension */,},
      {"",}, {"",}, {"",}, 
      {"auto",  SCSPEC, RID_AUTO,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
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
