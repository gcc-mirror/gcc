/* C code produced by gperf version 2.7.1 (19981006 egcs) */
/* Command-line: gperf -L C -F , 0, 0 -p -j1 -g -o -t -N is_reserved_word -k1,4,7,$ ../../../gcc/cp/gxx.gperf  */
/* Command-line: gperf -L KR-C -F ', 0, 0' -p -j1 -g -o -t -N is_reserved_word -k1,4,$,7 gplus.gperf  */
struct resword { const char *name; short token; enum rid rid;};

#define TOTAL_KEYWORDS 106
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 16
#define MIN_HASH_VALUE 4
#define MAX_HASH_VALUE 250
/* maximum key range = 247, duplicates = 0 */

#ifdef __GNUC__
__inline
#endif
static unsigned int
hash (str, len)
     register const char *str;
     register unsigned int len;
{
  static unsigned char asso_values[] =
    {
      251, 251, 251, 251, 251, 251, 251, 251, 251, 251,
      251, 251, 251, 251, 251, 251, 251, 251, 251, 251,
      251, 251, 251, 251, 251, 251, 251, 251, 251, 251,
      251, 251, 251, 251, 251, 251, 251, 251, 251, 251,
      251, 251, 251, 251, 251, 251, 251, 251, 251, 251,
      251, 251, 251, 251, 251, 251, 251, 251, 251, 251,
      251, 251, 251, 251, 251, 251, 251, 251, 251, 251,
      251, 251, 251, 251, 251, 251, 251, 251, 251, 251,
      251, 251, 251, 251, 251, 251, 251, 251, 251, 251,
      251, 251, 251, 251, 251,   0, 251,  64,  93,   3,
        0,   0,  74,  35,   0,  26, 251,   2,  31,  65,
       23,  76,   7,  19,  45,  37,   6,  64,  12,  38,
       14,   4, 251, 251, 251, 251, 251, 251, 251, 251,
      251, 251, 251, 251, 251, 251, 251, 251, 251, 251,
      251, 251, 251, 251, 251, 251, 251, 251, 251, 251,
      251, 251, 251, 251, 251, 251, 251, 251, 251, 251,
      251, 251, 251, 251, 251, 251, 251, 251, 251, 251,
      251, 251, 251, 251, 251, 251, 251, 251, 251, 251,
      251, 251, 251, 251, 251, 251, 251, 251, 251, 251,
      251, 251, 251, 251, 251, 251, 251, 251, 251, 251,
      251, 251, 251, 251, 251, 251, 251, 251, 251, 251,
      251, 251, 251, 251, 251, 251, 251, 251, 251, 251,
      251, 251, 251, 251, 251, 251, 251, 251, 251, 251,
      251, 251, 251, 251, 251, 251, 251, 251, 251, 251,
      251, 251, 251, 251, 251, 251, 251, 251, 251, 251,
      251, 251, 251, 251, 251, 251
    };
  register int hval = len;

  switch (hval)
    {
      default:
      case 7:
        hval += asso_values[(unsigned char)str[6]];
      case 6:
      case 5:
      case 4:
        hval += asso_values[(unsigned char)str[3]];
      case 3:
      case 2:
      case 1:
        hval += asso_values[(unsigned char)str[0]];
        break;
    }
  return hval + asso_values[(unsigned char)str[len - 1]];
}

#ifdef __GNUC__
__inline
#endif
struct resword *
is_reserved_word (str, len)
     register const char *str;
     register unsigned int len;
{
  static struct resword wordlist[] =
    {
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"else", ELSE, NORID,},
      {"", 0, 0},
      {"delete", DELETE, NORID,},
      {"case", CASE, NORID,},
      {"__real__", REALPART, NORID},
      {"", 0, 0},
      {"true", CXX_TRUE, NORID,},
      {"catch", CATCH, NORID,},
      {"typeid", TYPEID, NORID,},
      {"try", TRY, NORID,},
      {"", 0, 0}, {"", 0, 0},
      {"void", TYPESPEC, RID_VOID,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"private", VISSPEC, RID_PRIVATE,},
      {"template", TEMPLATE, RID_TEMPLATE,},
      {"protected", VISSPEC, RID_PROTECTED,},
      {"extern", SCSPEC, RID_EXTERN,},
      {"", 0, 0}, {"", 0, 0},
      {"not", '!', NORID,},
      {"", 0, 0},
      {"__signed", TYPESPEC, RID_SIGNED},
      {"int", TYPESPEC, RID_INT,},
      {"__signed__", TYPESPEC, RID_SIGNED},
      {"__real", REALPART, NORID},
      {"", 0, 0},
      {"xor_eq", ASSIGN, NORID,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"__attribute", ATTRIBUTE, NORID},
      {"__asm__", ASM_KEYWORD, NORID},
      {"__attribute__", ATTRIBUTE, NORID},
      {"compl", '~', NORID,},
      {"public", VISSPEC, RID_PUBLIC,},
      {"not_eq", EQCOMPARE, NORID,},
      {"switch", SWITCH, NORID,},
      {"__extension__", EXTENSION, NORID},
      {"const", CV_QUALIFIER, RID_CONST,},
      {"static", SCSPEC, RID_STATIC,},
      {"", 0, 0},
      {"__inline", SCSPEC, RID_INLINE},
      {"", 0, 0},
      {"__inline__", SCSPEC, RID_INLINE},
      {"__restrict__", CV_QUALIFIER, RID_RESTRICT},
      {"inline", SCSPEC, RID_INLINE,},
      {"const_cast", CONST_CAST, NORID,},
      {"static_cast", STATIC_CAST, NORID,},
      {"__restrict", CV_QUALIFIER, RID_RESTRICT},
      {"xor", '^', NORID,},
      {"__wchar_t", TYPESPEC, RID_WCHAR  /* Unique to ANSI C++ */,},
      {"new", NEW, NORID,},
      {"__alignof__", ALIGNOF, NORID},
      {"signed", TYPESPEC, RID_SIGNED,},
      {"and", ANDAND, NORID,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"explicit", SCSPEC, RID_EXPLICIT,},
      {"", 0, 0},
      {"__imag__", IMAGPART, NORID},
      {"while", WHILE, NORID,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"do", DO, NORID,},
      {"typename", TYPENAME_KEYWORD, NORID,},
      {"friend", SCSPEC, RID_FRIEND,},
      {"continue", CONTINUE, NORID,},
      {"class", AGGR, RID_CLASS,},
      {"default", DEFAULT, NORID,},
      {"this", THIS, NORID,},
      {"dynamic_cast", DYNAMIC_CAST, NORID,},
      {"typeof", TYPEOF, NORID,},
      {"virtual", SCSPEC, RID_VIRTUAL,},
      {"export", SCSPEC, RID_EXPORT,},
      {"and_eq", ASSIGN, NORID,},
      {"__typeof__", TYPEOF, NORID},
      {"__const__", CV_QUALIFIER, RID_CONST},
      {"__volatile", CV_QUALIFIER, RID_VOLATILE},
      {"short", TYPESPEC, RID_SHORT,},
      {"__volatile__", CV_QUALIFIER, RID_VOLATILE},
      {"__const", CV_QUALIFIER, RID_CONST},
      {"namespace", NAMESPACE, NORID,},
      {"char", TYPESPEC, RID_CHAR,},
      {"unsigned", TYPESPEC, RID_UNSIGNED,},
      {"double", TYPESPEC, RID_DOUBLE,},
      {"or_eq", ASSIGN, NORID,},
      {"__null", CONSTANT, RID_NULL},
      {"if", IF, NORID,},
      {"__signature__", AGGR, RID_SIGNATURE	/* Extension */,},
      {"__label__", LABEL, NORID},
      {"long", TYPESPEC, RID_LONG,},
      {"__imag", IMAGPART, NORID},
      {"__asm", ASM_KEYWORD, NORID},
      {"", 0, 0},
      {"__sigof__", SIGOF, NORID		/* Extension */,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"struct", AGGR, RID_RECORD,},
      {"", 0, 0},
      {"volatile", CV_QUALIFIER, RID_VOLATILE,},
      {"false", CXX_FALSE, NORID,},
      {"sizeof", SIZEOF, NORID,},
      {"__complex__", TYPESPEC, RID_COMPLEX},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"for", FOR, NORID,},
      {"or", OROR, NORID,},
      {"register", SCSPEC, RID_REGISTER,},
      {"throw", THROW, NORID,},
      {"", 0, 0},
      {"using", USING, NORID,},
      {"", 0, 0}, {"", 0, 0},
      {"__complex", TYPESPEC, RID_COMPLEX},
      {"", 0, 0},
      {"asm", ASM_KEYWORD, NORID,},
      {"signature", AGGR, RID_SIGNATURE	/* Extension */,},
      {"enum", ENUM, NORID,},
      {"reinterpret_cast", REINTERPRET_CAST, NORID,},
      {"mutable", SCSPEC, RID_MUTABLE,},
      {"__alignof", ALIGNOF, NORID},
      {"return", RETURN_KEYWORD, NORID,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0},
      {"float", TYPESPEC, RID_FLOAT,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"bool", TYPESPEC, RID_BOOL,},
      {"", 0, 0},
      {"typedef", SCSPEC, RID_TYPEDEF,},
      {"__typeof", TYPEOF, NORID},
      {"bitand", '&', NORID,},
      {"break", BREAK, NORID,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"union", AGGR, RID_UNION,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"goto", GOTO, NORID,},
      {"sigof", SIGOF, NORID		/* Extension */,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"bitor", '|', NORID,},
      {"auto", SCSPEC, RID_AUTO,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0},
      {"operator", OPERATOR, NORID,}
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register const char *s = wordlist[key].name;

          if (*str == *s && !strcmp (str + 1, s + 1))
            return &wordlist[key];
        }
    }
  return 0;
}
