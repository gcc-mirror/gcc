/* C code produced by gperf version 2.7.1 (19981006 egcs) */
/* Command-line: gperf -L C -F , 0, 0 -p -j1 -i 1 -g -o -t -G -N is_reserved_word -k1,3,$ ./c-parse.gperf  */
/* Command-line: gperf -L KR-C -F ', 0, 0' -p -j1 -i 1 -g -o -t -N is_reserved_word -k1,3,$ c-parse.gperf  */ 
struct resword { const char *name; short token; enum rid rid; };

#define TOTAL_KEYWORDS 83
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 20
#define MIN_HASH_VALUE 8
#define MAX_HASH_VALUE 141
/* maximum key range = 134, duplicates = 0 */

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
      142, 142, 142, 142, 142, 142, 142, 142, 142, 142,
      142, 142, 142, 142, 142, 142, 142, 142, 142, 142,
      142, 142, 142, 142, 142, 142, 142, 142, 142, 142,
      142, 142, 142, 142, 142, 142, 142, 142, 142, 142,
      142, 142, 142, 142, 142, 142, 142, 142, 142, 142,
      142, 142, 142, 142, 142, 142, 142, 142, 142, 142,
      142, 142, 142, 142,  35, 142, 142, 142, 142, 142,
      142, 142, 142, 142, 142, 142, 142, 142, 142, 142,
      142, 142, 142, 142, 142, 142, 142, 142, 142, 142,
      142, 142, 142, 142, 142,   1, 142,  90,   1,  28,
       40,   6,   1,  24,   3,  13, 142,  36,  60,  14,
       49,   3,   6, 142,  19,   8,   1,  50,  33,  11,
        2,  23,   4, 142, 142, 142, 142, 142, 142, 142,
      142, 142, 142, 142, 142, 142, 142, 142, 142, 142,
      142, 142, 142, 142, 142, 142, 142, 142, 142, 142,
      142, 142, 142, 142, 142, 142, 142, 142, 142, 142,
      142, 142, 142, 142, 142, 142, 142, 142, 142, 142,
      142, 142, 142, 142, 142, 142, 142, 142, 142, 142,
      142, 142, 142, 142, 142, 142, 142, 142, 142, 142,
      142, 142, 142, 142, 142, 142, 142, 142, 142, 142,
      142, 142, 142, 142, 142, 142, 142, 142, 142, 142,
      142, 142, 142, 142, 142, 142, 142, 142, 142, 142,
      142, 142, 142, 142, 142, 142, 142, 142, 142, 142,
      142, 142, 142, 142, 142, 142, 142, 142, 142, 142,
      142, 142, 142, 142, 142, 142, 142, 142, 142, 142,
      142, 142, 142, 142, 142, 142
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
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"out", TYPE_QUAL, RID_OUT},
    {"", 0, 0},
    {"float", TYPESPEC, RID_FLOAT},
    {"__typeof", TYPEOF, NORID},
    {"", 0, 0},
    {"__typeof__", TYPEOF, NORID},
    {"typeof", TYPEOF, NORID},
    {"typedef", SCSPEC, RID_TYPEDEF},
    {"if", IF, NORID},
    {"short", TYPESPEC, RID_SHORT},
    {"int", TYPESPEC, RID_INT},
    {"sizeof", SIZEOF, NORID},
    {"__signed__", TYPESPEC, RID_SIGNED},
    {"__extension__", EXTENSION, NORID},
    {"inout", TYPE_QUAL, RID_INOUT},
    {"__imag__", IMAGPART, NORID},
    {"else", ELSE, NORID},
    {"__inline__", SCSPEC, RID_INLINE},
    {"byref", TYPE_QUAL, RID_BYREF},
    {"__iterator__", SCSPEC, RID_ITERATOR},
    {"__inline", SCSPEC, RID_INLINE},
    {"__real__", REALPART, NORID},
    {"switch", SWITCH, NORID},
    {"__restrict", TYPE_QUAL, RID_RESTRICT},
    {"goto", GOTO, NORID},
    {"__restrict__", TYPE_QUAL, RID_RESTRICT},
    {"struct", STRUCT, NORID},
    {"while", WHILE, NORID},
    {"restrict", TYPE_QUAL, RID_RESTRICT},
    {"__const", TYPE_QUAL, RID_CONST},
    {"oneway", TYPE_QUAL, RID_ONEWAY},
    {"__const__", TYPE_QUAL, RID_CONST},
    {"__complex", TYPESPEC, RID_COMPLEX},
    {"__complex__", TYPESPEC, RID_COMPLEX},
    {"for", FOR, NORID},
    {"__iterator", SCSPEC, RID_ITERATOR},
    {"__imag", IMAGPART, NORID},
    {"do", DO, NORID},
    {"case", CASE, NORID},
    {"__volatile__", TYPE_QUAL, RID_VOLATILE},
    {"break", BREAK, NORID},
    {"default", DEFAULT, NORID},
    {"__volatile", TYPE_QUAL, RID_VOLATILE},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"@defs", DEFS, NORID},
    {"id", OBJECTNAME, RID_ID},
    {"", 0, 0},
    {"__signed", TYPESPEC, RID_SIGNED},
    {"bycopy", TYPE_QUAL, RID_BYCOPY},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"extern", SCSPEC, RID_EXTERN},
    {"", 0, 0},
    {"in", TYPE_QUAL, RID_IN},
    {"", 0, 0},
    {"@compatibility_alias", ALIAS, NORID},
    {"", 0, 0},
    {"@private", PRIVATE, NORID},
    {"@selector", SELECTOR, NORID},
    {"register", SCSPEC, RID_REGISTER},
    {"__label__", LABEL, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"enum", ENUM, NORID},
    {"return", RETURN, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"signed", TYPESPEC, RID_SIGNED},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"const", TYPE_QUAL, RID_CONST},
    {"", 0, 0},
    {"inline", SCSPEC, RID_INLINE},
    {"__real", REALPART, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"void", TYPESPEC, RID_VOID},
    {"continue", CONTINUE, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"@encode", ENCODE, NORID},
    {"auto", SCSPEC, RID_AUTO},
    {"__asm__", ASM_KEYWORD, NORID},
    {"@interface", INTERFACE, NORID},
    {"__alignof", ALIGNOF, NORID},
    {"double", TYPESPEC, RID_DOUBLE},
    {"__alignof__", ALIGNOF, NORID},
    {"@protected", PROTECTED, NORID},
    {"__attribute__", ATTRIBUTE, NORID},
    {"unsigned", TYPESPEC, RID_UNSIGNED},
    {"volatile", TYPE_QUAL, RID_VOLATILE},
    {"__attribute", ATTRIBUTE, NORID},
    {"@class", CLASS, NORID},
    {"__asm", ASM_KEYWORD, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"@implementation", IMPLEMENTATION, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"union", UNION, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"@public", PUBLIC, NORID},
    {"asm", ASM_KEYWORD, NORID},
    {"", 0, 0},
    {"@protocol", PROTOCOL, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"@end", END, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"static", SCSPEC, RID_STATIC},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"long", TYPESPEC, RID_LONG},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"char", TYPESPEC, RID_CHAR}
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
