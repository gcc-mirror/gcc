/* KR-C code produced by gperf version 2.7.1 (19981006 egcs) */
/* Command-line: gperf -L KR-C -F , 0, 0 -p -j1 -i 1 -g -o -t -G -N is_reserved_word -k1,3,$ ./c-parse.gperf  */
/* Command-line: gperf -L KR-C -F ', 0, 0' -p -j1 -i 1 -g -o -t -N is_reserved_word -k1,3,$ c-parse.gperf  */ 
struct resword { char *name; short token; enum rid rid; };

#define TOTAL_KEYWORDS 79
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 20
#define MIN_HASH_VALUE 10
#define MAX_HASH_VALUE 144
/* maximum key range = 135, duplicates = 0 */

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
      145, 145, 145, 145, 145, 145, 145, 145, 145, 145,
      145, 145, 145, 145, 145, 145, 145, 145, 145, 145,
      145, 145, 145, 145, 145, 145, 145, 145, 145, 145,
      145, 145, 145, 145, 145, 145, 145, 145, 145, 145,
      145, 145, 145, 145, 145, 145, 145, 145, 145, 145,
      145, 145, 145, 145, 145, 145, 145, 145, 145, 145,
      145, 145, 145, 145,  25, 145, 145, 145, 145, 145,
      145, 145, 145, 145, 145, 145, 145, 145, 145, 145,
      145, 145, 145, 145, 145, 145, 145, 145, 145, 145,
      145, 145, 145, 145, 145,   1, 145,  46,   8,  15,
       61,   6,  36,  48,   3,   5, 145,  18,  63,  25,
       29,  76,   1, 145,  13,   2,   1,  51,  37,   9,
        9,   1,   3, 145, 145, 145, 145, 145, 145, 145,
      145, 145, 145, 145, 145, 145, 145, 145, 145, 145,
      145, 145, 145, 145, 145, 145, 145, 145, 145, 145,
      145, 145, 145, 145, 145, 145, 145, 145, 145, 145,
      145, 145, 145, 145, 145, 145, 145, 145, 145, 145,
      145, 145, 145, 145, 145, 145, 145, 145, 145, 145,
      145, 145, 145, 145, 145, 145, 145, 145, 145, 145,
      145, 145, 145, 145, 145, 145, 145, 145, 145, 145,
      145, 145, 145, 145, 145, 145, 145, 145, 145, 145,
      145, 145, 145, 145, 145, 145, 145, 145, 145, 145,
      145, 145, 145, 145, 145, 145, 145, 145, 145, 145,
      145, 145, 145, 145, 145, 145, 145, 145, 145, 145,
      145, 145, 145, 145, 145, 145, 145, 145, 145, 145,
      145, 145, 145, 145, 145, 145
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
    {"__signed__", TYPESPEC, RID_SIGNED},
    {"__imag__", IMAGPART, NORID},
    {"switch", SWITCH, NORID},
    {"__inline__", SCSPEC, RID_INLINE},
    {"else", ELSE, NORID},
    {"__iterator__", SCSPEC, RID_ITERATOR},
    {"__inline", SCSPEC, RID_INLINE},
    {"__extension__", EXTENSION, NORID},
    {"struct", STRUCT, NORID},
    {"__real__", REALPART, NORID},
    {"__const", TYPE_QUAL, RID_CONST},
    {"while", WHILE, NORID},
    {"__const__", TYPE_QUAL, RID_CONST},
    {"case", CASE, NORID},
    {"__complex__", TYPESPEC, RID_COMPLEX},
    {"__iterator", SCSPEC, RID_ITERATOR},
    {"bycopy", TYPE_QUAL, RID_BYCOPY},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"__complex", TYPESPEC, RID_COMPLEX},
    {"", 0, 0},
    {"in", TYPE_QUAL, RID_IN},
    {"break", BREAK, NORID},
    {"@defs", DEFS, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"extern", SCSPEC, RID_EXTERN},
    {"if", IF, NORID},
    {"typeof", TYPEOF, NORID},
    {"typedef", SCSPEC, RID_TYPEDEF},
    {"__typeof", TYPEOF, NORID},
    {"sizeof", SIZEOF, NORID},
    {"", 0, 0},
    {"return", RETURN, NORID},
    {"const", TYPE_QUAL, RID_CONST},
    {"__volatile__", TYPE_QUAL, RID_VOLATILE},
    {"@private", PRIVATE, NORID},
    {"@selector", SELECTOR, NORID},
    {"__volatile", TYPE_QUAL, RID_VOLATILE},
    {"__asm__", ASM_KEYWORD, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"continue", CONTINUE, NORID},
    {"__alignof__", ALIGNOF, NORID},
    {"__imag", IMAGPART, NORID},
    {"__attribute__", ATTRIBUTE, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"__attribute", ATTRIBUTE, NORID},
    {"for", FOR, NORID},
    {"", 0, 0},
    {"@encode", ENCODE, NORID},
    {"id", OBJECTNAME, RID_ID},
    {"static", SCSPEC, RID_STATIC},
    {"@interface", INTERFACE, NORID},
    {"", 0, 0},
    {"__signed", TYPESPEC, RID_SIGNED},
    {"", 0, 0},
    {"__label__", LABEL, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"__asm", ASM_KEYWORD, NORID},
    {"char", TYPESPEC, RID_CHAR},
    {"", 0, 0},
    {"inline", SCSPEC, RID_INLINE},
    {"out", TYPE_QUAL, RID_OUT},
    {"register", SCSPEC, RID_REGISTER},
    {"__real", REALPART, NORID},
    {"short", TYPESPEC, RID_SHORT},
    {"", 0, 0},
    {"enum", ENUM, NORID},
    {"inout", TYPE_QUAL, RID_INOUT},
    {"", 0, 0},
    {"oneway", TYPE_QUAL, RID_ONEWAY},
    {"union", UNION, NORID},
    {"", 0, 0},
    {"__alignof", ALIGNOF, NORID},
    {"", 0, 0},
    {"@implementation", IMPLEMENTATION, NORID},
    {"", 0, 0},
    {"@class", CLASS, NORID},
    {"", 0, 0},
    {"@public", PUBLIC, NORID},
    {"asm", ASM_KEYWORD, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"default", DEFAULT, NORID},
    {"", 0, 0},
    {"void", TYPESPEC, RID_VOID},
    {"", 0, 0},
    {"@protected", PROTECTED, NORID},
    {"@protocol", PROTOCOL, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"volatile", TYPE_QUAL, RID_VOLATILE},
    {"", 0, 0}, {"", 0, 0},
    {"signed", TYPESPEC, RID_SIGNED},
    {"float", TYPESPEC, RID_FLOAT},
    {"@end", END, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"unsigned", TYPESPEC, RID_UNSIGNED},
    {"@compatibility_alias", ALIAS, NORID},
    {"double", TYPESPEC, RID_DOUBLE},
    {"", 0, 0}, {"", 0, 0},
    {"auto", SCSPEC, RID_AUTO},
    {"", 0, 0},
    {"goto", GOTO, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"do", DO, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
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
