/* C code produced by gperf version 2.7.1 (19981006 egcs) */
/* Command-line: gperf -L C -F , 0, 0 -p -j1 -g -o -t -N is_reserved_word -k1,4,7,$ ../../../gcc/cp/gxx.gperf  */
/* Command-line: gperf -L C -F ', 0, 0' -p -j1 -g -o -t -N is_reserved_word -k1,4,$,7 gplus.gperf  */
struct resword { const char *name; short token; enum rid rid;};
#ifdef __GNUC__
__inline
#endif
static unsigned int hash PARAMS ((const char *, unsigned int));
#ifdef __GNUC__
__inline
#endif
struct resword *is_reserved_word PARAMS ((const char *, unsigned int));

#define TOTAL_KEYWORDS 103
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 16
#define MIN_HASH_VALUE 4
#define MAX_HASH_VALUE 274
/* maximum key range = 271, duplicates = 0 */

#ifdef __GNUC__
__inline
#endif
static unsigned int
hash (str, len)
     register const char *str;
     register unsigned int len;
{
  static unsigned short asso_values[] =
    {
      275, 275, 275, 275, 275, 275, 275, 275, 275, 275,
      275, 275, 275, 275, 275, 275, 275, 275, 275, 275,
      275, 275, 275, 275, 275, 275, 275, 275, 275, 275,
      275, 275, 275, 275, 275, 275, 275, 275, 275, 275,
      275, 275, 275, 275, 275, 275, 275, 275, 275, 275,
      275, 275, 275, 275, 275, 275, 275, 275, 275, 275,
      275, 275, 275, 275, 275, 275, 275, 275, 275, 275,
      275, 275, 275, 275, 275, 275, 275, 275, 275, 275,
      275, 275, 275, 275, 275, 275, 275, 275, 275, 275,
      275, 275, 275, 275, 275,   0, 275,  97,   1,  19,
       40,   0,  12,  68,   0,  74, 275,   0,  11,  67,
       27,   0,  70,   6,  96,  43,   6,  37,   3,  10,
        8, 104, 275, 275, 275, 275, 275, 275, 275, 275,
      275, 275, 275, 275, 275, 275, 275, 275, 275, 275,
      275, 275, 275, 275, 275, 275, 275, 275, 275, 275,
      275, 275, 275, 275, 275, 275, 275, 275, 275, 275,
      275, 275, 275, 275, 275, 275, 275, 275, 275, 275,
      275, 275, 275, 275, 275, 275, 275, 275, 275, 275,
      275, 275, 275, 275, 275, 275, 275, 275, 275, 275,
      275, 275, 275, 275, 275, 275, 275, 275, 275, 275,
      275, 275, 275, 275, 275, 275, 275, 275, 275, 275,
      275, 275, 275, 275, 275, 275, 275, 275, 275, 275,
      275, 275, 275, 275, 275, 275, 275, 275, 275, 275,
      275, 275, 275, 275, 275, 275, 275, 275, 275, 275,
      275, 275, 275, 275, 275, 275, 275, 275, 275, 275,
      275, 275, 275, 275, 275, 275
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
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"__real__", REALPART, RID_UNUSED},
      {"", 0, 0},
      {"true", CXX_TRUE, RID_UNUSED,},
      {"or_eq", ASSIGN, RID_UNUSED,},
      {"export", SCSPEC, RID_EXPORT,},
      {"", 0, 0}, {"", 0, 0},
      {"__const__", CV_QUALIFIER, RID_CONST},
      {"__volatile", CV_QUALIFIER, RID_VOLATILE},
      {"__real", REALPART, RID_UNUSED},
      {"__volatile__", CV_QUALIFIER, RID_VOLATILE},
      {"__const", CV_QUALIFIER, RID_CONST},
      {"xor_eq", ASSIGN, RID_UNUSED,},
      {"throw", THROW, RID_UNUSED,},
      {"__complex__", TYPESPEC, RID_COMPLEX},
      {"case", CASE, RID_UNUSED,},
      {"typeof", TYPEOF, RID_UNUSED,},
      {"", 0, 0},
      {"while", WHILE, RID_UNUSED,},
      {"bool", TYPESPEC, RID_BOOL,},
      {"__complex", TYPESPEC, RID_COMPLEX},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"extern", SCSPEC, RID_EXTERN,},
      {"", 0, 0}, {"", 0, 0},
      {"not", '!', RID_UNUSED,},
      {"typedef", SCSPEC, RID_TYPEDEF,},
      {"virtual", SCSPEC, RID_VIRTUAL,},
      {"not_eq", EQCOMPARE, RID_UNUSED,},
      {"new", NEW, RID_UNUSED,},
      {"", 0, 0},
      {"do", DO, RID_UNUSED,},
      {"catch", CATCH, RID_UNUSED,},
      {"", 0, 0}, {"", 0, 0},
      {"delete", DELETE, RID_UNUSED,},
      {"double", TYPESPEC, RID_DOUBLE,},
      {"__extension__", EXTENSION, RID_UNUSED},
      {"__alignof__", ALIGNOF, RID_UNUSED},
      {"__asm__", ASM_KEYWORD, RID_UNUSED},
      {"", 0, 0},
      {"typeid", TYPEID, RID_UNUSED,},
      {"", 0, 0},
      {"__null", CONSTANT, RID_NULL},
      {"switch", SWITCH, RID_UNUSED,},
      {"", 0, 0}, {"", 0, 0},
      {"friend", SCSPEC, RID_FRIEND,},
      {"__alignof", ALIGNOF, RID_UNUSED},
      {"false", CXX_FALSE, RID_UNUSED,},
      {"sizeof", SIZEOF, RID_UNUSED,},
      {"__inline", SCSPEC, RID_INLINE},
      {"", 0, 0},
      {"__inline__", SCSPEC, RID_INLINE},
      {"", 0, 0},
      {"static_cast", STATIC_CAST, RID_UNUSED,},
      {"", 0, 0}, {"", 0, 0},
      {"union", AGGR, RID_UNION,},
      {"continue", CONTINUE, RID_UNUSED,},
      {"", 0, 0},
      {"goto", GOTO, RID_UNUSED,},
      {"const", CV_QUALIFIER, RID_CONST,},
      {"static", SCSPEC, RID_STATIC,},
      {"__imag__", IMAGPART, RID_UNUSED},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"private", VISSPEC, RID_PRIVATE,},
      {"typename", TYPENAME_KEYWORD, RID_UNUSED,},
      {"", 0, 0},
      {"int", TYPESPEC, RID_INT,},
      {"__signed__", TYPESPEC, RID_SIGNED},
      {"", 0, 0}, {"", 0, 0},
      {"void", TYPESPEC, RID_VOID,},
      {"if", IF, RID_UNUSED,},
      {"", 0, 0},
      {"template", TEMPLATE, RID_TEMPLATE,},
      {"__attribute", ATTRIBUTE, RID_UNUSED},
      {"struct", AGGR, RID_RECORD,},
      {"__attribute__", ATTRIBUTE, RID_UNUSED},
      {"", 0, 0}, {"", 0, 0},
      {"this", THIS, RID_UNUSED,},
      {"const_cast", CONST_CAST, RID_UNUSED,},
      {"or", OROR, RID_UNUSED,},
      {"explicit", SCSPEC, RID_EXPLICIT,},
      {"", 0, 0},
      {"auto", SCSPEC, RID_AUTO,},
      {"bitor", '|', RID_UNUSED,},
      {"break", BREAK, RID_UNUSED,},
      {"", 0, 0},
      {"compl", '~', RID_UNUSED,},
      {"public", VISSPEC, RID_PUBLIC,},
      {"xor", '^', RID_UNUSED,},
      {"__restrict__", CV_QUALIFIER, RID_RESTRICT},
      {"and_eq", ASSIGN, RID_UNUSED,},
      {"class", AGGR, RID_CLASS,},
      {"for", FOR, RID_UNUSED,},
      {"__restrict", CV_QUALIFIER, RID_RESTRICT},
      {"try", TRY, RID_UNUSED,},
      {"__typeof__", TYPEOF, RID_UNUSED},
      {"__asm", ASM_KEYWORD, RID_UNUSED},
      {"signed", TYPESPEC, RID_SIGNED,},
      {"__label__", LABEL, RID_UNUSED},
      {"", 0, 0},
      {"volatile", CV_QUALIFIER, RID_VOLATILE,},
      {"float", TYPESPEC, RID_FLOAT,},
      {"", 0, 0},
      {"__signed", TYPESPEC, RID_SIGNED},
      {"", 0, 0},
      {"__typeof", TYPEOF, RID_UNUSED},
      {"", 0, 0}, {"", 0, 0},
      {"__builtin_va_arg", VA_ARG, RID_UNUSED},
      {"", 0, 0}, {"", 0, 0},
      {"__wchar_t", TYPESPEC, RID_WCHAR  /* Unique to ANSI C++ */,},
      {"protected", VISSPEC, RID_PROTECTED,},
      {"", 0, 0},
      {"namespace", NAMESPACE, RID_UNUSED,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"using", USING, RID_UNUSED,},
      {"enum", ENUM, RID_UNUSED,},
      {"", 0, 0},
      {"and", ANDAND, RID_UNUSED,},
      {"__imag", IMAGPART, RID_UNUSED},
      {"", 0, 0}, {"", 0, 0},
      {"bitand", '&', RID_UNUSED,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"short", TYPESPEC, RID_SHORT,},
      {"long", TYPESPEC, RID_LONG,},
      {"", 0, 0}, {"", 0, 0},
      {"inline", SCSPEC, RID_INLINE,},
      {"", 0, 0},
      {"default", DEFAULT, RID_UNUSED,},
      {"", 0, 0}, {"", 0, 0},
      {"unsigned", TYPESPEC, RID_UNSIGNED,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"return", RETURN_KEYWORD, RID_UNUSED,},
      {"asm", ASM_KEYWORD, RID_UNUSED,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"mutable", SCSPEC, RID_MUTABLE,},
      {"", 0, 0}, {"", 0, 0},
      {"dynamic_cast", DYNAMIC_CAST, RID_UNUSED,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"operator", OPERATOR, RID_UNUSED,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"char", TYPESPEC, RID_CHAR,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"reinterpret_cast", REINTERPRET_CAST, RID_UNUSED,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"register", SCSPEC, RID_REGISTER,}
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
