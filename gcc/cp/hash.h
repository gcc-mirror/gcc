/* C code produced by gperf version 2.5 (GNU C++ version) */
/* Command-line: gperf -p -j1 -g -o -t -N is_reserved_word -k1,4,7,$ ../../../devo/gcc/cp/gxx.gperf  */
/* Command-line: gperf -p -j1 -g -o -t -N is_reserved_word -k1,4,$,7 gplus.gperf  */
struct resword { char *name; short token; enum rid rid;};

#define TOTAL_KEYWORDS 97
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 16
#define MIN_HASH_VALUE 4
#define MAX_HASH_VALUE 202
/* maximum key range = 199, duplicates = 0 */

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
     203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
     203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
     203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
     203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
     203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
     203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
     203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
     203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
     203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
     203, 203, 203, 203, 203,   0, 203,  29,  22,  32,
      35,   0,  73,   8,  19,  48, 203,   0,   7,  15,
      11,  66,   9,  11,  19,  58,   1,   7,  83,  21,
      89,   5, 203, 203, 203, 203, 203, 203,
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
      {"",}, {"",}, {"",}, 
      {"try",  TRY, NORID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"not",  '!', NORID,},
      {"",}, 
      {"extern",  SCSPEC, RID_EXTERN,},
      {"",}, 
      {"template",  TEMPLATE, RID_TEMPLATE,},
      {"__null",  CONSTANT, RID_NULL},
      {"",}, {"",}, {"",}, 
      {"typename",  TYPENAME_KEYWORD, NORID,},
      {"",}, {"",}, 
      {"long",  TYPESPEC, RID_LONG,},
      {"not_eq",  EQCOMPARE, NORID,},
      {"__alignof__",  ALIGNOF, NORID},
      {"__inline",  SCSPEC, RID_INLINE},
      {"using",  USING, NORID,},
      {"__inline__",  SCSPEC, RID_INLINE},
      {"while",  WHILE, NORID,},
      {"enum",  ENUM, NORID,},
      {"new",  NEW, NORID,},
      {"case",  CASE, NORID,},
      {"",}, {"",}, {"",}, 
      {"bool",  TYPESPEC, RID_BOOL,},
      {"delete",  DELETE, NORID,},
      {"typeid",  TYPEID, NORID,},
      {"return",  RETURN, NORID,},
      {"",}, 
      {"__label__",  LABEL, NORID},
      {"and_eq",  ASSIGN, NORID,},
      {"asm",  ASM_KEYWORD, NORID,},
      {"continue",  CONTINUE, NORID,},
      {"namespace",  NAMESPACE, NORID,},
      {"",}, 
      {"mutable",  SCSPEC, RID_MUTABLE,},
      {"int",  TYPESPEC, RID_INT,},
      {"compl",  '~', NORID,},
      {"public",  VISSPEC, RID_PUBLIC,},
      {"protected",  VISSPEC, RID_PROTECTED,},
      {"break",  BREAK, NORID,},
      {"",}, 
      {"__signed__",  TYPESPEC, RID_SIGNED},
      {"",}, 
      {"__attribute",  ATTRIBUTE, NORID},
      {"__wchar_t",  TYPESPEC, RID_WCHAR  /* Unique to ANSI C++ */,},
      {"__attribute__",  ATTRIBUTE, NORID},
      {"double",  TYPESPEC, RID_DOUBLE,},
      {"explicit",  SCSPEC, RID_EXPLICIT,},
      {"__asm__",  GCC_ASM_KEYWORD, NORID},
      {"reinterpret_cast",  REINTERPRET_CAST, NORID,},
      {"and",  ANDAND, NORID,},
      {"",}, {"",}, {"",}, 
      {"static_cast",  STATIC_CAST, NORID,},
      {"struct",  AGGR, RID_RECORD,},
      {"default",  DEFAULT, NORID,},
      {"char",  TYPESPEC, RID_CHAR,},
      {"__const",  CV_QUALIFIER, RID_CONST},
      {"__const__",  CV_QUALIFIER, RID_CONST},
      {"__volatile",  CV_QUALIFIER, RID_VOLATILE},
      {"__asm",  GCC_ASM_KEYWORD, NORID},
      {"__volatile__",  CV_QUALIFIER, RID_VOLATILE},
      {"typeof",  TYPEOF, NORID,},
      {"__typeof__",  TYPEOF, NORID},
      {"or_eq",  ASSIGN, NORID,},
      {"short",  TYPESPEC, RID_SHORT,},
      {"switch",  SWITCH, NORID,},
      {"signature",  AGGR, RID_SIGNATURE	/* Extension */,},
      {"",}, 
      {"or",  OROR, NORID,},
      {"catch",  CATCH, NORID,},
      {"union",  AGGR, RID_UNION,},
      {"__signature__",  AGGR, RID_SIGNATURE	/* Extension */,},
      {"__signed",  TYPESPEC, RID_SIGNED},
      {"bitand",  '&', NORID,},
      {"throw",  THROW, NORID,},
      {"register",  SCSPEC, RID_REGISTER,},
      {"for",  FOR, NORID,},
      {"const",  CV_QUALIFIER, RID_CONST,},
      {"static",  SCSPEC, RID_STATIC,},
      {"unsigned",  TYPESPEC, RID_UNSIGNED,},
      {"private",  VISSPEC, RID_PRIVATE,},
      {"__alignof",  ALIGNOF, NORID},
      {"",}, 
      {"inline",  SCSPEC, RID_INLINE,},
      {"do",  DO, NORID,},
      {"",}, 
      {"virtual",  SCSPEC, RID_VIRTUAL,},
      {"xor_eq",  ASSIGN, NORID,},
      {"",}, 
      {"float",  TYPESPEC, RID_FLOAT,},
      {"dynamic_cast",  DYNAMIC_CAST, NORID,},
      {"signed",  TYPESPEC, RID_SIGNED,},
      {"xor",  '^', NORID,},
      {"bitor",  '|', NORID,},
      {"__extension__",  EXTENSION, NORID},
      {"friend",  SCSPEC, RID_FRIEND,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"this",  THIS, NORID,},
      {"",}, 
      {"if",  IF, NORID,},
      {"",}, {"",}, {"",}, 
      {"volatile",  CV_QUALIFIER, RID_VOLATILE,},
      {"",}, {"",}, 
      {"__sigof__",  SIGOF, NORID		/* Extension */,},
      {"",}, {"",}, 
      {"const_cast",  CONST_CAST, NORID,},
      {"",}, {"",}, 
      {"false",  CXX_FALSE, NORID,},
      {"sizeof",  SIZEOF, NORID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"goto",  GOTO, NORID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"__typeof",  TYPEOF, NORID},
      {"class",  AGGR, RID_CLASS,},
      {"typedef",  SCSPEC, RID_TYPEDEF,},
      {"",}, {"",}, 
      {"void",  TYPESPEC, RID_VOID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"auto",  SCSPEC, RID_AUTO,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, {"",}, 
      {"operator",  OPERATOR, NORID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"sigof",  SIGOF, NORID		/* Extension */,},
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
