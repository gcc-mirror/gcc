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
      {"else", ELSE, RID_UNUSED,},
      {"", 0, 0},
      {"delete", DELETE, RID_UNUSED,},
      {"case", CASE, RID_UNUSED,},
      {"__real__", REALPART, RID_UNUSED},
      {"", 0, 0},
      {"true", CXX_TRUE, RID_UNUSED,},
      {"catch", CATCH, RID_UNUSED,},
      {"typeid", TYPEID, RID_UNUSED,},
      {"try", TRY, RID_UNUSED,},
      {"", 0, 0}, {"", 0, 0},
      {"void", TYPESPEC, RID_VOID,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"private", VISSPEC, RID_PRIVATE,},
      {"template", TEMPLATE, RID_TEMPLATE,},
      {"protected", VISSPEC, RID_PROTECTED,},
      {"extern", SCSPEC, RID_EXTERN,},
      {"", 0, 0}, {"", 0, 0},
      {"not", '!', RID_UNUSED,},
      {"", 0, 0},
      {"__signed", TYPESPEC, RID_SIGNED},
      {"int", TYPESPEC, RID_INT,},
      {"__signed__", TYPESPEC, RID_SIGNED},
      {"__real", REALPART, RID_UNUSED},
      {"", 0, 0},
      {"xor_eq", ASSIGN, RID_UNUSED,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"__attribute", ATTRIBUTE, RID_UNUSED},
      {"__asm__", ASM_KEYWORD, RID_UNUSED},
      {"__attribute__", ATTRIBUTE, RID_UNUSED},
      {"compl", '~', RID_UNUSED,},
      {"public", VISSPEC, RID_PUBLIC,},
      {"not_eq", EQCOMPARE, RID_UNUSED,},
      {"switch", SWITCH, RID_UNUSED,},
      {"__extension__", EXTENSION, RID_UNUSED},
      {"const", CV_QUALIFIER, RID_CONST,},
      {"static", SCSPEC, RID_STATIC,},
      {"", 0, 0},
      {"__inline", SCSPEC, RID_INLINE},
      {"", 0, 0},
      {"__inline__", SCSPEC, RID_INLINE},
      {"__restrict__", CV_QUALIFIER, RID_RESTRICT},
      {"inline", SCSPEC, RID_INLINE,},
      {"const_cast", CONST_CAST, RID_UNUSED,},
      {"static_cast", STATIC_CAST, RID_UNUSED,},
      {"__restrict", CV_QUALIFIER, RID_RESTRICT},
      {"xor", '^', RID_UNUSED,},
      {"__wchar_t", TYPESPEC, RID_WCHAR  /* Unique to ANSI C++ */,},
      {"new", NEW, RID_UNUSED,},
      {"__alignof__", ALIGNOF, RID_UNUSED},
      {"signed", TYPESPEC, RID_SIGNED,},
      {"and", ANDAND, RID_UNUSED,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"explicit", SCSPEC, RID_EXPLICIT,},
      {"", 0, 0},
      {"__imag__", IMAGPART, RID_UNUSED},
      {"while", WHILE, RID_UNUSED,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"do", DO, RID_UNUSED,},
      {"typename", TYPENAME_KEYWORD, RID_UNUSED,},
      {"friend", SCSPEC, RID_FRIEND,},
      {"continue", CONTINUE, RID_UNUSED,},
      {"class", AGGR, RID_CLASS,},
      {"default", DEFAULT, RID_UNUSED,},
      {"this", THIS, RID_UNUSED,},
      {"dynamic_cast", DYNAMIC_CAST, RID_UNUSED,},
      {"typeof", TYPEOF, RID_UNUSED,},
      {"virtual", SCSPEC, RID_VIRTUAL,},
      {"export", SCSPEC, RID_EXPORT,},
      {"and_eq", ASSIGN, RID_UNUSED,},
      {"__typeof__", TYPEOF, RID_UNUSED},
      {"__const__", CV_QUALIFIER, RID_CONST},
      {"__volatile", CV_QUALIFIER, RID_VOLATILE},
      {"short", TYPESPEC, RID_SHORT,},
      {"__volatile__", CV_QUALIFIER, RID_VOLATILE},
      {"__const", CV_QUALIFIER, RID_CONST},
      {"namespace", NAMESPACE, RID_UNUSED,},
      {"char", TYPESPEC, RID_CHAR,},
      {"unsigned", TYPESPEC, RID_UNSIGNED,},
      {"double", TYPESPEC, RID_DOUBLE,},
      {"or_eq", ASSIGN, RID_UNUSED,},
      {"__null", CONSTANT, RID_NULL},
      {"if", IF, RID_UNUSED,},
      {"__signature__", AGGR, RID_SIGNATURE	/* Extension */,},
      {"__label__", LABEL, RID_UNUSED},
      {"long", TYPESPEC, RID_LONG,},
      {"__imag", IMAGPART, RID_UNUSED},
      {"__asm", ASM_KEYWORD, RID_UNUSED},
      {"", 0, 0},
      {"__sigof__", SIGOF, RID_UNUSED		/* Extension */,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"struct", AGGR, RID_RECORD,},
      {"", 0, 0},
      {"volatile", CV_QUALIFIER, RID_VOLATILE,},
      {"false", CXX_FALSE, RID_UNUSED,},
      {"sizeof", SIZEOF, RID_UNUSED,},
      {"__complex__", TYPESPEC, RID_COMPLEX},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"for", FOR, RID_UNUSED,},
      {"or", OROR, RID_UNUSED,},
      {"register", SCSPEC, RID_REGISTER,},
      {"throw", THROW, RID_UNUSED,},
      {"", 0, 0},
      {"using", USING, RID_UNUSED,},
      {"", 0, 0}, {"", 0, 0},
      {"__complex", TYPESPEC, RID_COMPLEX},
      {"", 0, 0},
      {"asm", ASM_KEYWORD, RID_UNUSED,},
      {"signature", AGGR, RID_SIGNATURE	/* Extension */,},
      {"enum", ENUM, RID_UNUSED,},
      {"reinterpret_cast", REINTERPRET_CAST, RID_UNUSED,},
      {"mutable", SCSPEC, RID_MUTABLE,},
      {"__alignof", ALIGNOF, RID_UNUSED},
      {"return", RETURN_KEYWORD, RID_UNUSED,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0},
      {"float", TYPESPEC, RID_FLOAT,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"bool", TYPESPEC, RID_BOOL,},
      {"", 0, 0},
      {"typedef", SCSPEC, RID_TYPEDEF,},
      {"__typeof", TYPEOF, RID_UNUSED},
      {"bitand", '&', RID_UNUSED,},
      {"break", BREAK, RID_UNUSED,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"union", AGGR, RID_UNION,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"goto", GOTO, RID_UNUSED,},
      {"sigof", SIGOF, RID_UNUSED		/* Extension */,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"bitor", '|', RID_UNUSED,},
      {"auto", SCSPEC, RID_AUTO,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0},
      {"operator", OPERATOR, RID_UNUSED,}
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
