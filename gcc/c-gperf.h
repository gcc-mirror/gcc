/* C code produced by gperf version 2.7 */
/* Command-line: gperf -L C -F , 0, 0 -p -j1 -i 1 -g -o -t -G -N is_reserved_word -k1,3,$ ../../../egcs/gcc/c-parse.gperf  */
/* Command-line: gperf -L KR-C -F ', 0, 0' -p -j1 -i 1 -g -o -t -N is_reserved_word -k1,3,$ c-parse.gperf  */ 
struct resword { const char *name; short token; enum rid rid; };

#define TOTAL_KEYWORDS 84
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 20
#define MIN_HASH_VALUE 8
#define MAX_HASH_VALUE 173
/* maximum key range = 166, duplicates = 0 */

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
      174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
      174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
      174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
      174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
      174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
      174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
      174, 174, 174, 174,  35, 174, 174, 174, 174, 174,
      174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
      174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
      174, 174, 174, 174, 174,   1, 174,  97,  19,  28,
       40,   6,   1,  53,   3,  13, 174,   5,  67,  18,
       49,   3,   6, 174,  19,   8,   1,   4,  33,   2,
        2,  23,   4, 174, 174, 174, 174, 174, 174, 174,
      174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
      174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
      174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
      174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
      174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
      174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
      174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
      174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
      174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
      174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
      174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
      174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
      174, 174, 174, 174, 174, 174
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
    {"while", WHILE, NORID},
    {"__iterator__", SCSPEC, RID_ITERATOR},
    {"__inline", SCSPEC, RID_INLINE},
    {"__real__", REALPART, NORID},
    {"switch", SWITCH, NORID},
    {"__restrict", TYPE_QUAL, RID_RESTRICT},
    {"enum", ENUM, NORID},
    {"__restrict__", TYPE_QUAL, RID_RESTRICT},
    {"struct", STRUCT, NORID},
    {"break", BREAK, NORID},
    {"restrict", TYPE_QUAL, RID_RESTRICT},
    {"__const", TYPE_QUAL, RID_CONST},
    {"oneway", TYPE_QUAL, RID_ONEWAY},
    {"__const__", TYPE_QUAL, RID_CONST},
    {"__complex", TYPESPEC, RID_COMPLEX},
    {"__complex__", TYPESPEC, RID_COMPLEX},
    {"for", FOR, NORID},
    {"__iterator", SCSPEC, RID_ITERATOR},
    {"byref", TYPE_QUAL, RID_BYREF},
    {"do", DO, NORID},
    {"case", CASE, NORID},
    {"__volatile__", TYPE_QUAL, RID_VOLATILE},
    {"", 0, 0},
    {"default", DEFAULT, NORID},
    {"__volatile", TYPE_QUAL, RID_VOLATILE},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"@defs", DEFS, NORID},
    {"id", OBJECTNAME, RID_ID},
    {"double", TYPESPEC, RID_DOUBLE},
    {"__signed", TYPESPEC, RID_SIGNED},
    {"", 0, 0}, {"", 0, 0},
    {"unsigned", TYPESPEC, RID_UNSIGNED},
    {"goto", GOTO, NORID},
    {"extern", SCSPEC, RID_EXTERN},
    {"", 0, 0},
    {"in", TYPE_QUAL, RID_IN},
    {"", 0, 0},
    {"@compatibility_alias", ALIAS, NORID},
    {"", 0, 0},
    {"@private", PRIVATE, NORID},
    {"@selector", SELECTOR, NORID},
    {"", 0, 0},
    {"union", UNION, NORID},
    {"", 0, 0},
    {"__imag", IMAGPART, NORID},
    {"@public", PUBLIC, NORID},
    {"return", RETURN, NORID},
    {"bycopy", TYPE_QUAL, RID_BYCOPY},
    {"", 0, 0},
    {"__label__", LABEL, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"const", TYPE_QUAL, RID_CONST},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"__builtin_va_arg", VA_ARG, NORID},
    {"void", TYPESPEC, RID_VOID},
    {"continue", CONTINUE, NORID},
    {"inline", SCSPEC, RID_INLINE},
    {"__real", REALPART, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"@encode", ENCODE, NORID},
    {"", 0, 0},
    {"register", SCSPEC, RID_REGISTER},
    {"@interface", INTERFACE, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"@protected", PROTECTED, NORID},
    {"auto", SCSPEC, RID_AUTO},
    {"__asm__", ASM_KEYWORD, NORID},
    {"signed", TYPESPEC, RID_SIGNED},
    {"__alignof", ALIGNOF, NORID},
    {"", 0, 0},
    {"__alignof__", ALIGNOF, NORID},
    {"", 0, 0},
    {"__attribute__", ATTRIBUTE, NORID},
    {"", 0, 0},
    {"volatile", TYPE_QUAL, RID_VOLATILE},
    {"__attribute", ATTRIBUTE, NORID},
    {"@class", CLASS, NORID},
    {"@implementation", IMPLEMENTATION, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"__asm", ASM_KEYWORD, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"@end", END, NORID},
    {"", 0, 0},
    {"@protocol", PROTOCOL, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"asm", ASM_KEYWORD, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"static", SCSPEC, RID_STATIC},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"char", TYPESPEC, RID_CHAR},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
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
