/* C code produced by gperf version 2.7 */
/* Command-line: gperf -L C -F , 0, 0 -p -j1 -i 1 -g -o -t -G -N is_reserved_word -k1,3,$ ../../egcs/gcc/c-parse.gperf  */
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

#define TOTAL_KEYWORDS 92
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 20
#define MIN_HASH_VALUE 17
#define MAX_HASH_VALUE 301
/* maximum key range = 285, duplicates = 0 */

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
      302, 302, 302, 302, 302, 302, 302, 302, 302, 302,
      302, 302, 302, 302, 302, 302, 302, 302, 302, 302,
      302, 302, 302, 302, 302, 302, 302, 302, 302, 302,
      302, 302, 302, 302, 302, 302, 302, 302, 302, 302,
      302, 302, 302, 302, 302, 302, 302, 302, 302, 302,
      302, 302, 302, 302, 302, 302, 302, 302, 302, 302,
      302, 302, 302, 302, 113, 302, 302, 302, 302, 302,
      302, 302, 302, 302, 302, 302, 302, 302, 302, 302,
      302, 302, 302, 302, 302, 302, 302, 302, 302, 302,
      302, 302, 302, 302, 302,   1, 302,  78,  52, 111,
       34,   9,  46,  59,   1,  20, 302,   1, 118,  17,
       18,  39,  58, 302,   7,   6,  33,  70,  21,   2,
        5,   1,   1, 302, 302, 302, 302, 302, 302, 302,
      302, 302, 302, 302, 302, 302, 302, 302, 302, 302,
      302, 302, 302, 302, 302, 302, 302, 302, 302, 302,
      302, 302, 302, 302, 302, 302, 302, 302, 302, 302,
      302, 302, 302, 302, 302, 302, 302, 302, 302, 302,
      302, 302, 302, 302, 302, 302, 302, 302, 302, 302,
      302, 302, 302, 302, 302, 302, 302, 302, 302, 302,
      302, 302, 302, 302, 302, 302, 302, 302, 302, 302,
      302, 302, 302, 302, 302, 302, 302, 302, 302, 302,
      302, 302, 302, 302, 302, 302, 302, 302, 302, 302,
      302, 302, 302, 302, 302, 302, 302, 302, 302, 302,
      302, 302, 302, 302, 302, 302, 302, 302, 302, 302,
      302, 302, 302, 302, 302, 302, 302, 302, 302, 302,
      302, 302, 302, 302, 302, 302
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
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"__real__", REALPART, NORID},
    {"__signed__", TYPESPEC, RID_SIGNED},
    {"", 0, 0}, {"", 0, 0},
    {"__restrict__", TYPE_QUAL, RID_RESTRICT},
    {"", 0, 0}, {"", 0, 0},
    {"__extension__", EXTENSION, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"else", ELSE, NORID},
    {"", 0, 0},
    {"__imag__", IMAGPART, NORID},
    {"", 0, 0},
    {"__inline__", SCSPEC, RID_INLINE},
    {"switch", SWITCH, NORID},
    {"", 0, 0},
    {"__volatile__", TYPE_QUAL, RID_VOLATILE},
    {"while", WHILE, NORID},
    {"", 0, 0},
    {"__inline", SCSPEC, RID_INLINE},
    {"", 0, 0},
    {"in", TYPE_QUAL, RID_IN},
    {"__volatile", TYPE_QUAL, RID_VOLATILE},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"__typeof__", TYPEOF, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"__signed", TYPESPEC, RID_SIGNED},
    {"", 0, 0},
    {"__restrict", TYPE_QUAL, RID_RESTRICT},
    {"struct", STRUCT, NORID},
    {"", 0, 0},
    {"restrict", TYPE_QUAL, RID_RESTRICT},
    {"oneway", TYPE_QUAL, RID_ONEWAY},
    {"id", OBJECTNAME, RID_ID},
    {"", 0, 0}, {"", 0, 0},
    {"sizeof", SIZEOF, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"for", FOR, NORID},
    {"return", RETURN, NORID},
    {"__bounded__", TYPE_QUAL, RID_BOUNDED},
    {"extern", SCSPEC, RID_EXTERN},
    {"break", BREAK, NORID},
    {"if", IF, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"__ptrbase__", PTR_BASE, NORID},
    {"__ptrvalue__", PTR_VALUE, NORID},
    {"__ptrextent__", PTR_EXTENT, NORID},
    {"", 0, 0},
    {"do", DO, NORID},
    {"", 0, 0},
    {"__ptrbase", PTR_BASE, NORID},
    {"__ptrvalue", PTR_VALUE, NORID},
    {"void", TYPESPEC, RID_VOID},
    {"", 0, 0},
    {"register", SCSPEC, RID_REGISTER},
    {"", 0, 0},
    {"short", TYPESPEC, RID_SHORT},
    {"", 0, 0},
    {"__unbounded__", TYPE_QUAL, RID_UNBOUNDED},
    {"__imag", IMAGPART, NORID},
    {"__asm__", ASM_KEYWORD, NORID},
    {"__typeof", TYPEOF, NORID},
    {"int", TYPESPEC, RID_INT},
    {"", 0, 0},
    {"__alignof__", ALIGNOF, NORID},
    {"", 0, 0},
    {"__attribute__", ATTRIBUTE, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"__bounded", TYPE_QUAL, RID_BOUNDED},
    {"inout", TYPE_QUAL, RID_INOUT},
    {"", 0, 0},
    {"__attribute", ATTRIBUTE, NORID},
    {"enum", ENUM, NORID},
    {"__asm", ASM_KEYWORD, NORID},
    {"", 0, 0},
    {"__ptrextent", PTR_EXTENT, NORID},
    {"", 0, 0},
    {"signed", TYPESPEC, RID_SIGNED},
    {"", 0, 0}, {"", 0, 0},
    {"out", TYPE_QUAL, RID_OUT},
    {"", 0, 0},
    {"byref", TYPE_QUAL, RID_BYREF},
    {"", 0, 0}, {"", 0, 0},
    {"union", UNION, NORID},
    {"", 0, 0},
    {"asm", ASM_KEYWORD, NORID},
    {"__unbounded", TYPE_QUAL, RID_UNBOUNDED},
    {"", 0, 0},
    {"unsigned", TYPESPEC, RID_UNSIGNED},
    {"double", TYPESPEC, RID_DOUBLE},
    {"default", DEFAULT, NORID},
    {"", 0, 0},
    {"__const__", TYPE_QUAL, RID_CONST},
    {"float", TYPESPEC, RID_FLOAT},
    {"__complex__", TYPESPEC, RID_COMPLEX},
    {"", 0, 0},
    {"__complex", TYPESPEC, RID_COMPLEX},
    {"", 0, 0},
    {"__builtin_va_arg", VA_ARG, NORID},
    {"__label__", LABEL, NORID},
    {"case", CASE, NORID},
    {"", 0, 0},
    {"__real", REALPART, NORID},
    {"@defs", DEFS, NORID},
    {"__alignof", ALIGNOF, NORID},
    {"goto", GOTO, NORID},
    {"", 0, 0},
    {"@private", PRIVATE, NORID},
    {"@selector", SELECTOR, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"typeof", TYPEOF, NORID},
    {"typedef", SCSPEC, RID_TYPEDEF},
    {"", 0, 0},
    {"continue", CONTINUE, NORID},
    {"@encode", ENCODE, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"@interface", INTERFACE, NORID},
    {"", 0, 0},
    {"__const", TYPE_QUAL, RID_CONST},
    {"inline", SCSPEC, RID_INLINE},
    {"auto", SCSPEC, RID_AUTO},
    {"", 0, 0},
    {"volatile", TYPE_QUAL, RID_VOLATILE},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"@implementation", IMPLEMENTATION, NORID},
    {"@protected", PROTECTED, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"const", TYPE_QUAL, RID_CONST},
    {"", 0, 0},
    {"@end", END, NORID},
    {"bycopy", TYPE_QUAL, RID_BYCOPY},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"@compatibility_alias", ALIAS, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0},
    {"long", TYPESPEC, RID_LONG},
    {"char", TYPESPEC, RID_CHAR},
    {"static", SCSPEC, RID_STATIC},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"@class", CLASS, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"@protocol", PROTOCOL, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"@public", PUBLIC, NORID}
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
