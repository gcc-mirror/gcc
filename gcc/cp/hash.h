/* KR-C code produced by gperf version 2.7.1 (19981006 egcs) */
/* Command-line: gperf -L KR-C -F , 0, 0 -p -j1 -g -o -t -N is_reserved_word -k1,4,7,$ ./gxx.gperf  */
/* Command-line: gperf -L KR-C -F ', 0, 0' -p -j1 -g -o -t -N is_reserved_word -k1,4,$,7 gplus.gperf  */
struct resword { char *name; short token; enum rid rid;};

#define TOTAL_KEYWORDS 104
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 16
#define MIN_HASH_VALUE 4
#define MAX_HASH_VALUE 261
/* maximum key range = 258, duplicates = 0 */

#ifdef __GNUC__
__inline
#endif
static unsigned int
hash (str, len)
     register char *str;
     register unsigned int len;
{
  static unsigned short asso_values[] =
    {
      262, 262, 262, 262, 262, 262, 262, 262, 262, 262,
      262, 262, 262, 262, 262, 262, 262, 262, 262, 262,
      262, 262, 262, 262, 262, 262, 262, 262, 262, 262,
      262, 262, 262, 262, 262, 262, 262, 262, 262, 262,
      262, 262, 262, 262, 262, 262, 262, 262, 262, 262,
      262, 262, 262, 262, 262, 262, 262, 262, 262, 262,
      262, 262, 262, 262, 262, 262, 262, 262, 262, 262,
      262, 262, 262, 262, 262, 262, 262, 262, 262, 262,
      262, 262, 262, 262, 262, 262, 262, 262, 262, 262,
      262, 262, 262, 262, 262,   0, 262,  87,  25,  96,
       60,   0,  55,   7,   4,  41, 262,   2,  15,  49,
       14,  63,  32,  29,   3,  23,   6,   8,   2,   2,
        0,   7, 262, 262, 262, 262, 262, 262, 262, 262,
      262, 262, 262, 262, 262, 262, 262, 262, 262, 262,
      262, 262, 262, 262, 262, 262, 262, 262, 262, 262,
      262, 262, 262, 262, 262, 262, 262, 262, 262, 262,
      262, 262, 262, 262, 262, 262, 262, 262, 262, 262,
      262, 262, 262, 262, 262, 262, 262, 262, 262, 262,
      262, 262, 262, 262, 262, 262, 262, 262, 262, 262,
      262, 262, 262, 262, 262, 262, 262, 262, 262, 262,
      262, 262, 262, 262, 262, 262, 262, 262, 262, 262,
      262, 262, 262, 262, 262, 262, 262, 262, 262, 262,
      262, 262, 262, 262, 262, 262, 262, 262, 262, 262,
      262, 262, 262, 262, 262, 262, 262, 262, 262, 262,
      262, 262, 262, 262, 262, 262, 262, 262, 262, 262,
      262, 262, 262, 262, 262, 262
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
     register char *str;
     register unsigned int len;
{
  static struct resword wordlist[] =
    {
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"else", ELSE, NORID,},
      {"", 0, 0},
      {"xor", '^', NORID,},
      {"", 0, 0},
      {"__real__", REALPART, NORID},
      {"", 0, 0},
      {"true", CXX_TRUE, NORID,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"try", TRY, NORID,},
      {"", 0, 0}, {"", 0, 0},
      {"new", NEW, NORID,},
      {"extern", SCSPEC, RID_EXTERN,},
      {"__real", REALPART, NORID},
      {"while", WHILE, NORID,},
      {"not", '!', NORID,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"__extension__", EXTENSION, NORID},
      {"", 0, 0},
      {"__null", CONSTANT, RID_NULL},
      {"__asm__", ASM_KEYWORD, NORID},
      {"return", RETURN, NORID,},
      {"", 0, 0},
      {"long", TYPESPEC, RID_LONG,},
      {"using", USING, NORID,},
      {"xor_eq", ASSIGN, NORID,},
      {"__inline", SCSPEC, RID_INLINE},
      {"short", TYPESPEC, RID_SHORT,},
      {"__inline__", SCSPEC, RID_INLINE},
      {"switch", SWITCH, NORID,},
      {"__alignof__", ALIGNOF, NORID},
      {"private", VISSPEC, RID_PRIVATE,},
      {"reinterpret_cast", REINTERPRET_CAST, NORID,},
      {"struct", AGGR, RID_RECORD,},
      {"", 0, 0},
      {"virtual", SCSPEC, RID_VIRTUAL,},
      {"static_cast", STATIC_CAST, NORID,},
      {"", 0, 0}, {"", 0, 0},
      {"not_eq", EQCOMPARE, NORID,},
      {"int", TYPESPEC, RID_INT,},
      {"__signed__", TYPESPEC, RID_SIGNED},
      {"template", TEMPLATE, RID_TEMPLATE,},
      {"", 0, 0},
      {"signature", AGGR, RID_SIGNATURE	/* Extension */,},
      {"register", SCSPEC, RID_REGISTER,},
      {"this", THIS, NORID,},
      {"__imag__", IMAGPART, NORID},
      {"__attribute", ATTRIBUTE, NORID},
      {"bool", TYPESPEC, RID_BOOL,},
      {"__attribute__", ATTRIBUTE, NORID},
      {"for", FOR, NORID,},
      {"__imag", IMAGPART, NORID},
      {"typename", TYPENAME_KEYWORD, NORID,},
      {"", 0, 0}, {"", 0, 0},
      {"delete", DELETE, NORID,},
      {"typeof", TYPEOF, NORID,},
      {"or", OROR, NORID,},
      {"", 0, 0},
      {"explicit", SCSPEC, RID_EXPLICIT,},
      {"", 0, 0},
      {"typeid", TYPEID, NORID,},
      {"", 0, 0}, {"", 0, 0},
      {"export", SCSPEC, RID_EXPORT,},
      {"throw", THROW, NORID,},
      {"__asm", ASM_KEYWORD, NORID},
      {"__const__", CV_QUALIFIER, RID_CONST},
      {"__volatile", CV_QUALIFIER, RID_VOLATILE},
      {"__typeof__", TYPEOF, NORID},
      {"__volatile__", CV_QUALIFIER, RID_VOLATILE},
      {"__const", CV_QUALIFIER, RID_CONST},
      {"false", CXX_FALSE, NORID,},
      {"sizeof", SIZEOF, NORID,},
      {"", 0, 0}, {"", 0, 0},
      {"__complex", TYPESPEC, RID_COMPLEX},
      {"inline", SCSPEC, RID_INLINE,},
      {"__complex__", TYPESPEC, RID_COMPLEX},
      {"union", AGGR, RID_UNION,},
      {"double", TYPESPEC, RID_DOUBLE,},
      {"", 0, 0},
      {"__alignof", ALIGNOF, NORID},
      {"", 0, 0}, {"", 0, 0},
      {"bitor", '|', NORID,},
      {"or_eq", ASSIGN, NORID,},
      {"if", IF, NORID,},
      {"", 0, 0},
      {"case", CASE, NORID,},
      {"", 0, 0},
      {"enum", ENUM, NORID,},
      {"signed", TYPESPEC, RID_SIGNED,},
      {"", 0, 0},
      {"__sigof__", SIGOF, NORID		/* Extension */,},
      {"char", TYPESPEC, RID_CHAR,},
      {"", 0, 0}, {"", 0, 0},
      {"__signed", TYPESPEC, RID_SIGNED},
      {"namespace", NAMESPACE, NORID,},
      {"__label__", LABEL, NORID},
      {"volatile", CV_QUALIFIER, RID_VOLATILE,},
      {"protected", VISSPEC, RID_PROTECTED,},
      {"__wchar_t", TYPESPEC, RID_WCHAR  /* Unique to ANSI C++ */,},
      {"", 0, 0}, {"", 0, 0},
      {"unsigned", TYPESPEC, RID_UNSIGNED,},
      {"continue", CONTINUE, NORID,},
      {"break", BREAK, NORID,},
      {"", 0, 0},
      {"friend", SCSPEC, RID_FRIEND,},
      {"and_eq", ASSIGN, NORID,},
      {"typedef", SCSPEC, RID_TYPEDEF,},
      {"", 0, 0},
      {"do", DO, NORID,},
      {"void", TYPESPEC, RID_VOID,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"const", CV_QUALIFIER, RID_CONST,},
      {"static", SCSPEC, RID_STATIC,},
      {"", 0, 0},
      {"__typeof", TYPEOF, NORID},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"goto", GOTO, NORID,},
      {"", 0, 0},
      {"asm", ASM_KEYWORD, NORID,},
      {"operator", OPERATOR, NORID,},
      {"__signature__", AGGR, RID_SIGNATURE	/* Extension */,},
      {"", 0, 0},
      {"mutable", SCSPEC, RID_MUTABLE,},
      {"", 0, 0}, {"", 0, 0},
      {"sigof", SIGOF, NORID		/* Extension */,},
      {"class", AGGR, RID_CLASS,},
      {"compl", '~', NORID,},
      {"public", VISSPEC, RID_PUBLIC,},
      {"and", ANDAND, NORID,},
      {"", 0, 0}, {"", 0, 0},
      {"float", TYPESPEC, RID_FLOAT,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"default", DEFAULT, NORID,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0},
      {"bitand", '&', NORID,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"catch", CATCH, NORID,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"auto", SCSPEC, RID_AUTO,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"const_cast", CONST_CAST, NORID,},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
      {"", 0, 0}, {"", 0, 0},
      {"dynamic_cast", DYNAMIC_CAST, NORID,}
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register char *s = wordlist[key].name;

          if (*str == *s && !strcmp (str + 1, s + 1))
            return &wordlist[key];
        }
    }
  return 0;
}
