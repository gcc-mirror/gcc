/* C code produced by gperf version 2.5 (GNU C++ version) */
/* Command-line: gperf -p -j1 -g -o -t -N is_reserved_word -k1,4,7,$ ../../../devo/gcc/cp/gxx.gperf  */
/* Command-line: gperf -p -j1 -g -o -t -N is_reserved_word -k1,4,$,7 gplus.gperf  */
struct resword { char *name; short token; enum rid rid;};

#define TOTAL_KEYWORDS 97
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 16
#define MIN_HASH_VALUE 4
#define MAX_HASH_VALUE 219
/* maximum key range = 216, duplicates = 0 */

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
     220, 220, 220, 220, 220, 220, 220, 220, 220, 220,
     220, 220, 220, 220, 220, 220, 220, 220, 220, 220,
     220, 220, 220, 220, 220, 220, 220, 220, 220, 220,
     220, 220, 220, 220, 220, 220, 220, 220, 220, 220,
     220, 220, 220, 220, 220, 220, 220, 220, 220, 220,
     220, 220, 220, 220, 220, 220, 220, 220, 220, 220,
     220, 220, 220, 220, 220, 220, 220, 220, 220, 220,
     220, 220, 220, 220, 220, 220, 220, 220, 220, 220,
     220, 220, 220, 220, 220, 220, 220, 220, 220, 220,
     220, 220, 220, 220, 220,   0, 220,  88,  16,  19,
      52,   0,   9,  72,   1,  77, 220,   0,   0,  38,
      13,  44,  38,  30,  27,  57,   1,  14,   0,   2,
       2,   7, 220, 220, 220, 220, 220, 220,
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
      {"true",  CXX_TRUE, NORID,},
      {"",}, 
      {"while",  WHILE, NORID,},
      {"virtual",  SCSPEC, RID_VIRTUAL,},
      {"",}, {"",}, 
      {"try",  TRY, NORID,},
      {"",}, {"",}, {"",}, {"",}, 
      {"typeof",  TYPEOF, NORID,},
      {"not",  '!', NORID,},
      {"new",  NEW, NORID,},
      {"extern",  SCSPEC, RID_EXTERN,},
      {"bool",  TYPESPEC, RID_BOOL,},
      {"",}, {"",}, 
      {"case",  CASE, NORID,},
      {"__alignof__",  ALIGNOF, NORID},
      {"",}, 
      {"typedef",  SCSPEC, RID_TYPEDEF,},
      {"",}, 
      {"__extension__",  EXTENSION, NORID},
      {"",}, {"",}, 
      {"__alignof",  ALIGNOF, NORID},
      {"xor",  '^', NORID,},
      {"",}, 
      {"__inline",  SCSPEC, RID_INLINE},
      {"",}, 
      {"__inline__",  SCSPEC, RID_INLINE},
      {"",}, 
      {"xor_eq",  ASSIGN, NORID,},
      {"for",  FOR, NORID,},
      {"",}, {"",}, 
      {"continue",  CONTINUE, NORID,},
      {"",}, 
      {"catch",  CATCH, NORID,},
      {"private",  VISSPEC, RID_PRIVATE,},
      {"",}, 
      {"typename",  TYPENAME_KEYWORD, NORID,},
      {"template",  TEMPLATE, RID_TEMPLATE,},
      {"not_eq",  EQCOMPARE, NORID,},
      {"",}, {"",}, 
      {"throw",  THROW, NORID,},
      {"__const",  TYPE_QUAL, RID_CONST},
      {"__const__",  TYPE_QUAL, RID_CONST},
      {"__volatile",  TYPE_QUAL, RID_VOLATILE},
      {"__wchar_t",  TYPESPEC, RID_WCHAR  /* Unique to ANSI C++ */,},
      {"__volatile__",  TYPE_QUAL, RID_VOLATILE},
      {"delete",  DELETE, NORID,},
      {"typeid",  TYPEID, NORID,},
      {"return",  RETURN, NORID,},
      {"__typeof__",  TYPEOF, NORID},
      {"compl",  '~', NORID,},
      {"public",  VISSPEC, RID_PUBLIC,},
      {"__asm__",  GCC_ASM_KEYWORD, NORID},
      {"switch",  SWITCH, NORID,},
      {"",}, 
      {"friend",  SCSPEC, RID_FRIEND,},
      {"__typeof",  TYPEOF, NORID},
      {"",}, 
      {"static_cast",  STATIC_CAST, NORID,},
      {"false",  CXX_FALSE, NORID,},
      {"sizeof",  SIZEOF, NORID,},
      {"or",  OROR, NORID,},
      {"double",  TYPESPEC, RID_DOUBLE,},
      {"",}, 
      {"union",  AGGR, RID_UNION,},
      {"char",  TYPESPEC, RID_CHAR,},
      {"struct",  AGGR, RID_RECORD,},
      {"or_eq",  ASSIGN, NORID,},
      {"enum",  ENUM, NORID,},
      {"int",  TYPESPEC, RID_INT,},
      {"const",  TYPE_QUAL, RID_CONST,},
      {"static",  SCSPEC, RID_STATIC,},
      {"reinterpret_cast",  REINTERPRET_CAST, NORID,},
      {"",}, 
      {"explicit",  SCSPEC, RID_EXPLICIT,},
      {"__signed__",  TYPESPEC, RID_SIGNED},
      {"if",  IF, NORID,},
      {"__attribute",  ATTRIBUTE, NORID},
      {"short",  TYPESPEC, RID_SHORT,},
      {"__attribute__",  ATTRIBUTE, NORID},
      {"bitor",  '|', NORID,},
      {"signature",  AGGR, RID_SIGNATURE	/* Extension */,},
      {"",}, 
      {"__sigof__",  SIGOF, NORID		/* Extension */,},
      {"volatile",  TYPE_QUAL, RID_VOLATILE,},
      {"__label__",  LABEL, NORID},
      {"do",  DO, NORID,},
      {"",}, 
      {"__asm",  GCC_ASM_KEYWORD, NORID},
      {"protected",  VISSPEC, RID_PROTECTED,},
      {"",}, 
      {"float",  TYPESPEC, RID_FLOAT,},
      {"using",  USING, NORID,},
      {"",}, 
      {"const_cast",  CONST_CAST, NORID,},
      {"",}, 
      {"void",  TYPESPEC, RID_VOID,},
      {"break",  BREAK, NORID,},
      {"namespace",  NAMESPACE, NORID,},
      {"",}, {"",}, {"",}, {"",}, 
      {"sigof",  SIGOF, NORID		/* Extension */,},
      {"",}, {"",}, {"",}, 
      {"this",  THIS, NORID,},
      {"",}, {"",}, {"",}, {"",}, 
      {"and_eq",  ASSIGN, NORID,},
      {"",}, {"",}, {"",}, 
      {"signed",  TYPESPEC, RID_SIGNED,},
      {"asm",  ASM_KEYWORD, NORID,},
      {"",}, {"",}, {"",}, 
      {"mutable",  SCSPEC, RID_MUTABLE,},
      {"",}, {"",}, {"",}, 
      {"__signed",  TYPESPEC, RID_SIGNED},
      {"class",  AGGR, RID_CLASS,},
      {"register",  SCSPEC, RID_REGISTER,},
      {"",}, {"",}, {"",}, 
      {"and",  ANDAND, NORID,},
      {"",}, {"",}, {"",}, {"",}, 
      {"long",  TYPESPEC, RID_LONG,},
      {"default",  DEFAULT, NORID,},
      {"operator",  OPERATOR, NORID,},
      {"unsigned",  TYPESPEC, RID_UNSIGNED,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"inline",  SCSPEC, RID_INLINE,},
      {"",}, 
      {"bitand",  '&', NORID,},
      {"",}, 
      {"goto",  GOTO, NORID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"dynamic_cast",  DYNAMIC_CAST, NORID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"__signature__",  AGGR, RID_SIGNATURE	/* Extension */,},
      {"",}, 
      {"auto",  SCSPEC, RID_AUTO,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, 
      {"overload",  OVERLOAD, NORID,},
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
