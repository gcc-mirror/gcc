/* C code produced by gperf version 2.7.1 (19981006 egcs) */
/* Command-line: gperf -L C -F , 0, 0 -p -j1 -i 1 -g -o -t -G -N is_reserved_word -k1,3,$ /work/src/gcc/gcc/c-parse.gperf  */
/* Command-line: gperf -L C -F ', 0, 0' -p -j1 -i 1 -g -o -t -N is_reserved_word -k1,3,$ c-parse.gperf  */ 
struct resword { const char *name; short token; enum rid rid; };
#ifdef __GNUC__
__inline
#endif
static unsigned int hash PARAMS ((const char *, unsigned int));
#ifdef __GNUC__
__inline
#endif
struct resword *is_reserved_word PARAMS ((const char *, unsigned int));

#define TOTAL_KEYWORDS 94
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 20
#define MIN_HASH_VALUE 4
#define MAX_HASH_VALUE 249
/* maximum key range = 246, duplicates = 0 */

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
      250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
      250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
      250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
      250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
      250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
      250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
      250, 250, 250, 250,  11, 250, 250, 250, 250, 250,
      250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
      250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
      250, 250, 250, 250, 250,   1, 250,  78,  38,  61,
        1,  37,  47,  70,   1,  13, 250,   4,  94,  37,
       81,   1, 100, 250,  19,   8,  25,   4,  50,   1,
        2,   1,   2, 250, 250, 250, 250, 250, 250, 250,
      250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
      250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
      250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
      250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
      250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
      250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
      250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
      250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
      250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
      250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
      250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
      250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
      250, 250, 250, 250, 250, 250
    };
  register int hval = len;

  switch (hval)
    {
      default:
      case 3:
        hval += asso_values[(unsigned char)str[2]];
      case 2:
      case 1:
        hval += asso_values[(unsigned char)str[0]];
        break;
    }
  return hval + asso_values[(unsigned char)str[len - 1]];
}

static struct resword wordlist[] =
  {
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"do", DO, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0},
    {"id", OBJECTNAME, RID_ID},
    {"__unbounded", TYPE_QUAL, RID_UNBOUNDED},
    {"__signed", TYPESPEC, RID_SIGNED},
    {"__unbounded__", TYPE_QUAL, RID_UNBOUNDED},
    {"__signed__", TYPESPEC, RID_SIGNED},
    {"unsigned", TYPESPEC, RID_UNSIGNED},
    {"", 0, 0},
    {"__imag__", IMAGPART, NORID},
    {"", 0, 0},
    {"__inline__", SCSPEC, RID_INLINE},
    {"", 0, 0},
    {"__iterator__", SCSPEC, RID_ITERATOR},
    {"switch", SWITCH, NORID},
    {"__real__", REALPART, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"__restrict__", TYPE_QUAL, RID_RESTRICT},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"__typeof__", TYPEOF, NORID},
    {"", 0, 0},
    {"short", TYPESPEC, RID_SHORT},
    {"@compatibility_alias", ALIAS, NORID},
    {"@protected", PROTECTED, NORID},
    {"", 0, 0},
    {"__iterator", SCSPEC, RID_ITERATOR},
    {"inout", TYPE_QUAL, RID_INOUT},
    {"oneway", TYPE_QUAL, RID_ONEWAY},
    {"", 0, 0}, {"", 0, 0},
    {"double", TYPESPEC, RID_DOUBLE},
    {"__bounded", TYPE_QUAL, RID_BOUNDED},
    {"", 0, 0},
    {"__bounded__", TYPE_QUAL, RID_BOUNDED},
    {"__extension__", EXTENSION, NORID},
    {"", 0, 0},
    {"out", TYPE_QUAL, RID_OUT},
    {"__restrict", TYPE_QUAL, RID_RESTRICT},
    {"while", WHILE, NORID},
    {"", 0, 0},
    {"struct", STRUCT, NORID},
    {"__inline", SCSPEC, RID_INLINE},
    {"restrict", TYPE_QUAL, RID_RESTRICT},
    {"@defs", DEFS, NORID},
    {"if", IF, NORID},
    {"sizeof", SIZEOF, NORID},
    {"__volatile__", TYPE_QUAL, RID_VOLATILE},
    {"", 0, 0},
    {"int", TYPESPEC, RID_INT},
    {"", 0, 0},
    {"void", TYPESPEC, RID_VOID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"__const__", TYPE_QUAL, RID_CONST},
    {"__complex", TYPESPEC, RID_COMPLEX},
    {"__complex__", TYPESPEC, RID_COMPLEX},
    {"@private", PRIVATE, NORID},
    {"@selector", SELECTOR, NORID},
    {"", 0, 0},
    {"float", TYPESPEC, RID_FLOAT},
    {"", 0, 0},
    {"default", DEFAULT, NORID},
    {"__typeof", TYPEOF, NORID},
    {"enum", ENUM, NORID},
    {"@public", PUBLIC, NORID},
    {"break", BREAK, NORID},
    {"signed", TYPESPEC, RID_SIGNED},
    {"else", ELSE, NORID},
    {"__asm__", ASM_KEYWORD, NORID},
    {"for", FOR, NORID},
    {"", 0, 0},
    {"__imag", IMAGPART, NORID},
    {"__alignof__", ALIGNOF, NORID},
    {"", 0, 0},
    {"__attribute__", ATTRIBUTE, NORID},
    {"__const", TYPE_QUAL, RID_CONST},
    {"", 0, 0},
    {"in", TYPE_QUAL, RID_IN},
    {"@end", END, NORID},
    {"__volatile", TYPE_QUAL, RID_VOLATILE},
    {"", 0, 0},
    {"goto", GOTO, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"union", UNION, NORID},
    {"", 0, 0},
    {"__label__", LABEL, NORID},
    {"bycopy", TYPE_QUAL, RID_BYCOPY},
    {"", 0, 0},
    {"auto", SCSPEC, RID_AUTO},
    {"byref", TYPE_QUAL, RID_BYREF},
    {"case", CASE, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"__ptrbase__", PTR_BASE, NORID},
    {"__ptrvalue__", PTR_VALUE, NORID},
    {"__ptrextent__", PTR_EXTENT, NORID},
    {"register", SCSPEC, RID_REGISTER},
    {"", 0, 0}, {"", 0, 0},
    {"@class", CLASS, NORID},
    {"__real", REALPART, NORID},
    {"__asm", ASM_KEYWORD, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"__builtin_va_arg", VA_ARG, NORID},
    {"", 0, 0},
    {"__attribute", ATTRIBUTE, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"return", RETURN, NORID},
    {"", 0, 0},
    {"@protocol", PROTOCOL, NORID},
    {"", 0, 0},
    {"__alignof", ALIGNOF, NORID},
    {"@encode", ENCODE, NORID},
    {"__ptrextent", PTR_EXTENT, NORID},
    {"", 0, 0},
    {"@interface", INTERFACE, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"@implementation", IMPLEMENTATION, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"__ptrbase", PTR_BASE, NORID},
    {"__ptrvalue", PTR_VALUE, NORID},
    {"extern", SCSPEC, RID_EXTERN},
    {"inline", SCSPEC, RID_INLINE},
    {"", 0, 0}, {"", 0, 0},
    {"static", SCSPEC, RID_STATIC},
    {"", 0, 0},
    {"asm", ASM_KEYWORD, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"char", TYPESPEC, RID_CHAR},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"const", TYPE_QUAL, RID_CONST},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"typeof", TYPEOF, NORID},
    {"typedef", SCSPEC, RID_TYPEDEF},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"continue", CONTINUE, NORID},
    {"", 0, 0},
    {"volatile", TYPE_QUAL, RID_VOLATILE},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"long", TYPESPEC, RID_LONG}
  };

#ifdef __GNUC__
__inline
#endif
struct resword *
is_reserved_word (str, len)
     register const char *str;
     register unsigned int len;
{
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
