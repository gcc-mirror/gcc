/* C code produced by gperf version 2.7 */
/* Command-line: gperf -L C -F , 0, 0 -p -j1 -g -o -t -N is_reserved_word -k1,4,7,$ ../../../../egcs/gcc/cp/gxx.gperf  */
/* Command-line: gperf -L KR-C -F ', 0, 0' -p -j1 -g -o -t -N is_reserved_word -k1,4,$,7 gplus.gperf  */
struct resword { const char *name; short token; enum rid rid;};

#define TOTAL_KEYWORDS 107
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 16
#define MIN_HASH_VALUE 4
#define MAX_HASH_VALUE 244
/* maximum key range = 241, duplicates = 0 */

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
      245, 245, 245, 245, 245, 245, 245, 245, 245, 245,
      245, 245, 245, 245, 245, 245, 245, 245, 245, 245,
      245, 245, 245, 245, 245, 245, 245, 245, 245, 245,
      245, 245, 245, 245, 245, 245, 245, 245, 245, 245,
      245, 245, 245, 245, 245, 245, 245, 245, 245, 245,
      245, 245, 245, 245, 245, 245, 245, 245, 245, 245,
      245, 245, 245, 245, 245, 245, 245, 245, 245, 245,
      245, 245, 245, 245, 245, 245, 245, 245, 245, 245,
      245, 245, 245, 245, 245, 245, 245, 245, 245, 245,
      245, 245, 245, 245, 245,   0, 245,  92,   3,   3,
        0,   0,  75,  24,   0,  90, 245,  10,  31,  49,
       23,  74,   0,  24,  30,  37,   6,  77,  10,  19,
        5,   4, 245, 245, 245, 245, 245, 245, 245, 245,
      245, 245, 245, 245, 245, 245, 245, 245, 245, 245,
      245, 245, 245, 245, 245, 245, 245, 245, 245, 245,
      245, 245, 245, 245, 245, 245, 245, 245, 245, 245,
      245, 245, 245, 245, 245, 245, 245, 245, 245, 245,
      245, 245, 245, 245, 245, 245, 245, 245, 245, 245,
      245, 245, 245, 245, 245, 245, 245, 245, 245, 245,
      245, 245, 245, 245, 245, 245, 245, 245, 245, 245,
      245, 245, 245, 245, 245, 245, 245, 245, 245, 245,
      245, 245, 245, 245, 245, 245, 245, 245, 245, 245,
      245, 245, 245, 245, 245, 245, 245, 245, 245, 245,
      245, 245, 245, 245, 245, 245, 245, 245, 245, 245,
      245, 245, 245, 245, 245, 245, 245, 245, 245, 245,
      245, 245, 245, 245, 245, 245
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
      {"double", TYPESPEC, RID_DOUBLE,},
      {"true", CXX_TRUE, RID_UNUSED,},
      {"catch", CATCH, RID_UNUSED,},
      {"typeid", TYPEID, RID_UNUSED,},
      {"try", TRY, RID_UNUSED,},
      {"void", TYPESPEC, RID_VOID,},
      {"", 0, 0}, {"", 0, 0},
      {"private", VISSPEC, RID_PRIVATE,},
      {"", 0, 0}, {"", 0, 0},
      {"template", TEMPLATE, RID_TEMPLATE,},
      {"protected", VISSPEC, RID_PROTECTED,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"extern", SCSPEC, RID_EXTERN,},
      {"", 0, 0}, {"", 0, 0},
      {"not", '!', RID_UNUSED,},
      {"", 0, 0}, {"", 0, 0},
      {"xor_eq", ASSIGN, RID_UNUSED,},
      {"", 0, 0},
      {"__real", REALPART, RID_UNUSED},
      {"xor", '^', RID_UNUSED,},
      {"compl", '~', RID_UNUSED,},
      {"public", VISSPEC, RID_PUBLIC,},
      {"__extension__", EXTENSION, RID_UNUSED},
      {"__restrict__", CV_QUALIFIER, RID_RESTRICT},
      {"", 0, 0},
      {"__asm__", ASM_KEYWORD, RID_UNUSED},
      {"new", NEW, RID_UNUSED,},
      {"__restrict", CV_QUALIFIER, RID_RESTRICT},
      {"", 0, 0},
      {"__wchar_t", TYPESPEC, RID_WCHAR  /* Unique to ANSI C++ */,},
      {"switch", SWITCH, RID_UNUSED,},
      {"", 0, 0},
      {"const", CV_QUALIFIER, RID_CONST,},
      {"static", SCSPEC, RID_STATIC,},
      {"not_eq", EQCOMPARE, RID_UNUSED,},
      {"__inline", SCSPEC, RID_INLINE},
      {"while", WHILE, RID_UNUSED,},
      {"__inline__", SCSPEC, RID_INLINE},
      {"__imag__", IMAGPART, RID_UNUSED},
      {"", 0, 0},
      {"const_cast", CONST_CAST, RID_UNUSED,},
      {"static_cast", STATIC_CAST, RID_UNUSED,},
      {"", 0, 0}, {"", 0, 0},
      {"typename", TYPENAME_KEYWORD, RID_UNUSED,},
      {"", 0, 0},
      {"__alignof__", ALIGNOF, RID_UNUSED},
      {"signed", TYPESPEC, RID_SIGNED,},
      {"char", TYPESPEC, RID_CHAR,},
      {"", 0, 0},
      {"bool", TYPESPEC, RID_BOOL,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"do", DO, RID_UNUSED,},
      {"", 0, 0},
      {"short", TYPESPEC, RID_SHORT,},
      {"__imag", IMAGPART, RID_UNUSED},
      {"", 0, 0},
      {"friend", SCSPEC, RID_FRIEND,},
      {"class", AGGR, RID_CLASS,},
      {"long", TYPESPEC, RID_LONG,},
      {"this", THIS, RID_UNUSED,},
      {"virtual", SCSPEC, RID_VIRTUAL,},
      {"export", SCSPEC, RID_EXPORT,},
      {"typeof", TYPEOF, RID_UNUSED,},
      {"__typeof__", TYPEOF, RID_UNUSED},
      {"__const__", CV_QUALIFIER, RID_CONST},
      {"__volatile", CV_QUALIFIER, RID_VOLATILE},
      {"__asm", ASM_KEYWORD, RID_UNUSED},
      {"__volatile__", CV_QUALIFIER, RID_VOLATILE},
      {"__const", CV_QUALIFIER, RID_CONST},
      {"continue", CONTINUE, RID_UNUSED,},
      {"and", ANDAND, RID_UNUSED,},
      {"", 0, 0}, {"", 0, 0},
      {"__signed", TYPESPEC, RID_SIGNED},
      {"int", TYPESPEC, RID_INT,},
      {"__signed__", TYPESPEC, RID_SIGNED},
      {"bitand", '&', RID_UNUSED,},
      {"enum", ENUM, RID_UNUSED,},
      {"or_eq", ASSIGN, RID_UNUSED,},
      {"throw", THROW, RID_UNUSED,},
      {"reinterpret_cast", REINTERPRET_CAST, RID_UNUSED,},
      {"or", OROR, RID_UNUSED,},
      {"__attribute", ATTRIBUTE, RID_UNUSED},
      {"for", FOR, RID_UNUSED,},
      {"__attribute__", ATTRIBUTE, RID_UNUSED},
      {"break", BREAK, RID_UNUSED,},
      {"default", DEFAULT, RID_UNUSED,},
      {"bitor", '|', RID_UNUSED,},
      {"dynamic_cast", DYNAMIC_CAST, RID_UNUSED,},
      {"__null", CONSTANT, RID_NULL},
      {"", 0, 0},
      {"__complex__", TYPESPEC, RID_COMPLEX},
      {"false", CXX_FALSE, RID_UNUSED,},
      {"sizeof", SIZEOF, RID_UNUSED,},
      {"__complex", TYPESPEC, RID_COMPLEX},
      {"", 0, 0}, {"", 0, 0},
      {"and_eq", ASSIGN, RID_UNUSED,},
      {"__builtin_va_arg", VA_ARG, RID_UNUSED},
      {"namespace", NAMESPACE, RID_UNUSED,},
      {"", 0, 0},
      {"struct", AGGR, RID_RECORD,},
      {"", 0, 0}, {"", 0, 0},
      {"using", USING, RID_UNUSED,},
      {"", 0, 0}, {"", 0, 0},
      {"__label__", LABEL, RID_UNUSED},
      {"", 0, 0}, {"", 0, 0},
      {"explicit", SCSPEC, RID_EXPLICIT,},
      {"return", RETURN_KEYWORD, RID_UNUSED,},
      {"", 0, 0},
      {"__alignof", ALIGNOF, RID_UNUSED},
      {"", 0, 0}, {"", 0, 0},
      {"volatile", CV_QUALIFIER, RID_VOLATILE,},
      {"", 0, 0}, {"", 0, 0},
      {"asm", ASM_KEYWORD, RID_UNUSED,},
      {"", 0, 0},
      {"signature", AGGR, RID_SIGNATURE	/* Extension */,},
      {"", 0, 0},
      {"mutable", SCSPEC, RID_MUTABLE,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"register", SCSPEC, RID_REGISTER,},
      {"", 0, 0}, {"", 0, 0},
      {"__typeof", TYPEOF, RID_UNUSED},
      {"", 0, 0},
      {"typedef", SCSPEC, RID_TYPEDEF,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"if", IF, RID_UNUSED,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"__sigof__", SIGOF, RID_UNUSED		/* Extension */,},
      {"unsigned", TYPESPEC, RID_UNSIGNED,},
      {"goto", GOTO, RID_UNUSED,},
      {"", 0, 0},
      {"float", TYPESPEC, RID_FLOAT,},
      {"union", AGGR, RID_UNION,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"inline", SCSPEC, RID_INLINE,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"sigof", SIGOF, RID_UNUSED		/* Extension */,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"__signature__", AGGR, RID_SIGNATURE	/* Extension */,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0},
      {"operator", OPERATOR, RID_UNUSED,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"auto", SCSPEC, RID_AUTO,}
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
