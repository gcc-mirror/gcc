/* KR-C code produced by gperf version 2.7.1 (19981006 egcs) */
/* Command-line: gperf -L KR-C -F , 0, 0 -p -j1 -i 1 -g -o -t -G -N is_reserved_word -k1,3,$ /hurl/puke/law/egcs/egcs/gcc/c-parse.gperf  */
/* Command-line: gperf -L KR-C -F ', 0, 0' -p -j1 -i 1 -g -o -t -N is_reserved_word -k1,3,$ c-parse.gperf  */ 
struct resword { char *name; short token; enum rid rid; };

#define TOTAL_KEYWORDS 80
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 20
#define MIN_HASH_VALUE 10
#define MAX_HASH_VALUE 162
/* maximum key range = 153, duplicates = 0 */

#ifdef __GNUC__
__inline
#endif
static unsigned int
hash (str, len)
     register char *str;
     register unsigned int len;
{
  static unsigned char asso_values[] =
    {
      163, 163, 163, 163, 163, 163, 163, 163, 163, 163,
      163, 163, 163, 163, 163, 163, 163, 163, 163, 163,
      163, 163, 163, 163, 163, 163, 163, 163, 163, 163,
      163, 163, 163, 163, 163, 163, 163, 163, 163, 163,
      163, 163, 163, 163, 163, 163, 163, 163, 163, 163,
      163, 163, 163, 163, 163, 163, 163, 163, 163, 163,
      163, 163, 163, 163,   8, 163, 163, 163, 163, 163,
      163, 163, 163, 163, 163, 163, 163, 163, 163, 163,
      163, 163, 163, 163, 163, 163, 163, 163, 163, 163,
      163, 163, 163, 163, 163,   1, 163,  24,   8,  61,
       37,   6,  47,  49,   2,   5, 163,   3,  51,  30,
       58,  91,  35, 163,  33,  13,   1,  18,  49,   2,
        2,   5,   3, 163, 163, 163, 163, 163, 163, 163,
      163, 163, 163, 163, 163, 163, 163, 163, 163, 163,
      163, 163, 163, 163, 163, 163, 163, 163, 163, 163,
      163, 163, 163, 163, 163, 163, 163, 163, 163, 163,
      163, 163, 163, 163, 163, 163, 163, 163, 163, 163,
      163, 163, 163, 163, 163, 163, 163, 163, 163, 163,
      163, 163, 163, 163, 163, 163, 163, 163, 163, 163,
      163, 163, 163, 163, 163, 163, 163, 163, 163, 163,
      163, 163, 163, 163, 163, 163, 163, 163, 163, 163,
      163, 163, 163, 163, 163, 163, 163, 163, 163, 163,
      163, 163, 163, 163, 163, 163, 163, 163, 163, 163,
      163, 163, 163, 163, 163, 163, 163, 163, 163, 163,
      163, 163, 163, 163, 163, 163, 163, 163, 163, 163,
      163, 163, 163, 163, 163, 163
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
    {"", 0, 0},
    {"int", TYPESPEC, RID_INT},
    {"", 0, 0}, {"", 0, 0},
    {"__typeof__", TYPEOF, NORID},
    {"", 0, 0},
    {"__imag__", IMAGPART, NORID},
    {"", 0, 0},
    {"__inline__", SCSPEC, RID_INLINE},
    {"while", WHILE, NORID},
    {"__iterator__", SCSPEC, RID_ITERATOR},
    {"__inline", SCSPEC, RID_INLINE},
    {"__extension__", EXTENSION, NORID},
    {"break", BREAK, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"__signed__", TYPESPEC, RID_SIGNED},
    {"switch", SWITCH, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"else", ELSE, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"@defs", DEFS, NORID},
    {"__asm__", ASM_KEYWORD, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"__alignof__", ALIGNOF, NORID},
    {"", 0, 0},
    {"__attribute__", ATTRIBUTE, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"__attribute", ATTRIBUTE, NORID},
    {"__real__", REALPART, NORID},
    {"id", OBJECTNAME, RID_ID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"__iterator", SCSPEC, RID_ITERATOR},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"struct", STRUCT, NORID},
    {"if", IF, NORID},
    {"@private", PRIVATE, NORID},
    {"@selector", SELECTOR, NORID},
    {"__typeof", TYPEOF, NORID},
    {"enum", ENUM, NORID},
    {"__signed", TYPESPEC, RID_SIGNED},
    {"__asm", ASM_KEYWORD, NORID},
    {"__imag", IMAGPART, NORID},
    {"__label__", LABEL, NORID},
    {"__volatile__", TYPE_QUAL, RID_VOLATILE},
    {"", 0, 0},
    {"in", TYPE_QUAL, RID_IN},
    {"__volatile", TYPE_QUAL, RID_VOLATILE},
    {"double", TYPESPEC, RID_DOUBLE},
    {"inline", SCSPEC, RID_INLINE},
    {"sizeof", SIZEOF, NORID},
    {"__const", TYPE_QUAL, RID_CONST},
    {"extern", SCSPEC, RID_EXTERN},
    {"__const__", TYPE_QUAL, RID_CONST},
    {"__complex", TYPESPEC, RID_COMPLEX},
    {"__complex__", TYPESPEC, RID_COMPLEX},
    {"", 0, 0},
    {"unsigned", TYPESPEC, RID_UNSIGNED},
    {"", 0, 0},
    {"@class", CLASS, NORID},
    {"@encode", ENCODE, NORID},
    {"bycopy", TYPE_QUAL, RID_BYCOPY},
    {"__alignof", ALIGNOF, NORID},
    {"@interface", INTERFACE, NORID},
    {"", 0, 0},
    {"case", CASE, NORID},
    {"", 0, 0},
    {"union", UNION, NORID},
    {"asm", ASM_KEYWORD, NORID},
    {"@protected", PROTECTED, NORID},
    {"typeof", TYPEOF, NORID},
    {"typedef", SCSPEC, RID_TYPEDEF},
    {"__real", REALPART, NORID},
    {"default", DEFAULT, NORID},
    {"byref", TYPE_QUAL, RID_BYREF},
    {"@public", PUBLIC, NORID},
    {"void", TYPESPEC, RID_VOID},
    {"out", TYPE_QUAL, RID_OUT},
    {"", 0, 0},
    {"return", RETURN, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"@protocol", PROTOCOL, NORID},
    {"inout", TYPE_QUAL, RID_INOUT},
    {"", 0, 0},
    {"static", SCSPEC, RID_STATIC},
    {"signed", TYPESPEC, RID_SIGNED},
    {"", 0, 0},
    {"@end", END, NORID},
    {"oneway", TYPE_QUAL, RID_ONEWAY},
    {"", 0, 0},
    {"short", TYPESPEC, RID_SHORT},
    {"@implementation", IMPLEMENTATION, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"volatile", TYPE_QUAL, RID_VOLATILE},
    {"", 0, 0},
    {"for", FOR, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"auto", SCSPEC, RID_AUTO},
    {"", 0, 0},
    {"char", TYPESPEC, RID_CHAR},
    {"register", SCSPEC, RID_REGISTER},
    {"", 0, 0},
    {"const", TYPE_QUAL, RID_CONST},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"do", DO, NORID},
    {"", 0, 0},
    {"@compatibility_alias", ALIAS, NORID},
    {"continue", CONTINUE, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0},
    {"float", TYPESPEC, RID_FLOAT},
    {"goto", GOTO, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"long", TYPESPEC, RID_LONG}
  };

#ifdef __GNUC__
__inline
#endif
struct resword *
is_reserved_word (str, len)
     register char *str;
     register unsigned int len;
{
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
