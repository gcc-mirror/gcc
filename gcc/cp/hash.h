/* C code produced by gperf version 2.5 (GNU C++ version) */
/* Command-line: gperf -p -j1 -g -o -t -N is_reserved_word -k1,4,7,$ /deneb/blob/jason/g++/small/devo/gcc/cp/gxx.gperf  */
/* Command-line: gperf -p -j1 -g -o -t -N is_reserved_word -k1,4,$,7 gplus.gperf  */
struct resword { char *name; short token; enum rid rid;};

#define TOTAL_KEYWORDS 83
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 16
#define MIN_HASH_VALUE 4
#define MAX_HASH_VALUE 170
/* maximum key range = 167, duplicates = 0 */

#ifdef __GNUC__
inline
#endif
static unsigned int
hash (str, len)
     register char *str;
     register int unsigned len;
{
  static unsigned char asso_values[] =
    {
     171, 171, 171, 171, 171, 171, 171, 171, 171, 171,
     171, 171, 171, 171, 171, 171, 171, 171, 171, 171,
     171, 171, 171, 171, 171, 171, 171, 171, 171, 171,
     171, 171, 171, 171, 171, 171, 171, 171, 171, 171,
     171, 171, 171, 171, 171, 171, 171, 171, 171, 171,
     171, 171, 171, 171, 171, 171, 171, 171, 171, 171,
     171, 171, 171, 171, 171, 171, 171, 171, 171, 171,
     171, 171, 171, 171, 171, 171, 171, 171, 171, 171,
     171, 171, 171, 171, 171, 171, 171, 171, 171, 171,
     171, 171, 171, 171, 171,   0, 171,  62,   5,  65,
      27,   0,  18,   7,  10,  48, 171,   1,  30,   7,
      79,   0,  33, 171,  18,   0,   4,  26,  13,   0,
       1,  24, 171, 171, 171, 171, 171, 171,
    };
  register int hval = len;

  switch (hval)
    {
      default:
      case 7:
        hval += asso_values[str[6]];
      case 6:
      case 5:
      case 4:
        hval += asso_values[str[3]];
      case 3:
      case 2:
      case 1:
        hval += asso_values[str[0]];
    }
  return hval + asso_values[str[len - 1]];
}

#ifdef __GNUC__
inline
#endif
struct resword *
is_reserved_word (str, len)
     register char *str;
     register unsigned int len;
{
  static struct resword wordlist[] =
    {
      {"",}, {"",}, {"",}, {"",}, 
      {"else",  ELSE, NORID,},
      {"",}, {"",}, 
      {"__asm__",  GCC_ASM_KEYWORD, NORID},
      {"this",  THIS, NORID,},
      {"throw",  THROW, NORID,},
      {"__headof__",  HEADOF, NORID},
      {"goto",  GOTO, NORID,},
      {"__asm",  GCC_ASM_KEYWORD, NORID},
      {"__const__",  TYPE_QUAL, RID_CONST},
      {"__volatile",  TYPE_QUAL, RID_VOLATILE},
      {"__const",  TYPE_QUAL, RID_CONST},
      {"__volatile__",  TYPE_QUAL, RID_VOLATILE},
      {"",}, 
      {"enum",  ENUM, NORID,},
      {"static_cast",  STATIC_CAST, NORID,},
      {"switch",  SWITCH, NORID,},
      {"",}, {"",}, 
      {"sigof",  SIGOF, NORID		/* Extension */,},
      {"sizeof",  SIZEOF, NORID,},
      {"",}, 
      {"__headof",  HEADOF, NORID},
      {"short",  TYPESPEC, RID_SHORT,},
      {"typeof",  TYPEOF, NORID,},
      {"do",  DO, NORID,},
      {"",}, 
      {"try",  TRY, NORID,},
      {"",}, 
      {"delete",  DELETE, NORID,},
      {"__typeof__",  TYPEOF, NORID},
      {"while",  WHILE, NORID,},
      {"struct",  AGGR, RID_RECORD,},
      {"typeid",  TYPEID, NORID,},
      {"double",  TYPESPEC, RID_DOUBLE,},
      {"for",  FOR, NORID,},
      {"",}, 
      {"__classof__",  CLASSOF, NORID},
      {"",}, {"",}, 
      {"operator",  OPERATOR, NORID,},
      {"",}, {"",}, 
      {"typedef",  SCSPEC, RID_TYPEDEF,},
      {"long",  TYPESPEC, RID_LONG,},
      {"template",  TEMPLATE, RID_TEMPLATE,},
      {"__typeof",  TYPEOF, NORID},
      {"friend",  SCSPEC, RID_FRIEND,},
      {"",}, 
      {"private",  VISSPEC, RID_PRIVATE,},
      {"",}, 
      {"int",  TYPESPEC, RID_INT,},
      {"",}, 
      {"__classof",  CLASSOF, NORID},
      {"__signed__",  TYPESPEC, RID_SIGNED},
      {"",}, {"",}, 
      {"headof",  HEADOF, NORID,},
      {"",}, 
      {"__attribute",  ATTRIBUTE, NORID},
      {"",}, 
      {"__attribute__",  ATTRIBUTE, NORID},
      {"auto",  SCSPEC, RID_AUTO,},
      {"",}, 
      {"if",  IF, NORID,},
      {"case",  CASE, NORID,},
      {"class",  AGGR, RID_CLASS,},
      {"void",  TYPESPEC, RID_VOID,},
      {"asm",  ASM_KEYWORD, NORID,},
      {"break",  BREAK, NORID,},
      {"const",  TYPE_QUAL, RID_CONST,},
      {"static",  SCSPEC, RID_STATIC,},
      {"mutable",  SCSPEC, RID_MUTABLE,},
      {"protected",  VISSPEC, RID_PROTECTED,},
      {"",}, {"",}, {"",}, {"",}, 
      {"new",  NEW, NORID,},
      {"__signed",  TYPESPEC, RID_SIGNED},
      {"virtual",  SCSPEC, RID_VIRTUAL,},
      {"extern",  SCSPEC, RID_EXTERN,},
      {"",}, {"",}, {"",}, 
      {"float",  TYPESPEC, RID_FLOAT,},
      {"",}, {"",}, 
      {"register",  SCSPEC, RID_REGISTER,},
      {"__extension__",  EXTENSION, NORID},
      {"",}, {"",}, 
      {"__wchar_t",  TYPESPEC, RID_WCHAR  /* Unique to ANSI C++ */,},
      {"",}, {"",}, {"",}, {"",}, 
      {"__label__",  LABEL, NORID},
      {"inline",  SCSPEC, RID_INLINE,},
      {"continue",  CONTINUE, NORID,},
      {"default",  DEFAULT, NORID,},
      {"char",  TYPESPEC, RID_CHAR,},
      {"",}, {"",}, 
      {"classof",  CLASSOF, NORID,},
      {"unsigned",  TYPESPEC, RID_UNSIGNED,},
      {"union",  AGGR, RID_UNION,},
      {"",}, 
      {"signed",  TYPESPEC, RID_SIGNED,},
      {"volatile",  TYPE_QUAL, RID_VOLATILE,},
      {"signature",  AGGR, RID_SIGNATURE	/* Extension */,},
      {"overload",  OVERLOAD, NORID,},
      {"",}, {"",}, {"",}, {"",}, 
      {"__alignof__",  ALIGNOF, NORID},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"return",  RETURN, NORID,},
      {"",}, {"",}, {"",}, {"",}, 
      {"public",  VISSPEC, RID_PUBLIC,},
      {"reinterpret_cast",  REINTERPRET_CAST, NORID,},
      {"__alignof",  ALIGNOF, NORID},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"const_cast",  CONST_CAST, NORID,},
      {"catch",  CATCH, NORID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, 
      {"__inline",  SCSPEC, RID_INLINE},
      {"",}, 
      {"__inline__",  SCSPEC, RID_INLINE},
      {"",}, 
      {"dynamic_cast",  DYNAMIC_CAST, NORID,},
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register char *s = wordlist[key].name;

          if (*s == *str && !strcmp (str + 1, s + 1))
            return &wordlist[key];
        }
    }
  return 0;
}
