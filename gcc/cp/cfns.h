/* C code produced by gperf version 2.7.2 */
/* Command-line: gperf -o -C -E -k '1-6,$' -j1 -D -N libc_name_p ../../../src-3.3/gcc/cp/cfns.gperf  */
#ifdef __GNUC__
__inline
#endif
static unsigned int hash PARAMS ((const char *, unsigned int));
#ifdef __GNUC__
__inline
#endif
const char * libc_name_p PARAMS ((const char *, unsigned int));
/* maximum key range = 480, duplicates = 1 */

#ifdef __GNUC__
__inline
#else
#ifdef __cplusplus
inline
#endif
#endif
static unsigned int
hash (str, len)
     register const char *str;
     register unsigned int len;
{
  static const unsigned short asso_values[] =
    {
      483, 483, 483, 483, 483, 483, 483, 483, 483, 483,
      483, 483, 483, 483, 483, 483, 483, 483, 483, 483,
      483, 483, 483, 483, 483, 483, 483, 483, 483, 483,
      483, 483, 483, 483, 483, 483, 483, 483, 483, 483,
      483, 483, 483, 483, 483, 483, 483, 483,   0,   0,
        1, 483, 483, 483, 483, 483, 483, 483, 483, 483,
      483, 483, 483, 483, 483, 483, 483, 483, 483, 483,
      483, 483, 483, 483, 483, 483, 483, 483, 483, 483,
      483, 483, 483, 483, 483, 483, 483, 483, 483, 483,
      483, 483, 483, 483, 483, 483, 483, 106,  76,   0,
       21,   0,   0,  11,  43,  26,   0,  66, 106,  17,
      121,   0,  17,   0,   7,   0,   3,  19,  49,   1,
        3,  41, 483, 483, 483, 483, 483, 483, 483, 483,
      483, 483, 483, 483, 483, 483, 483, 483, 483, 483,
      483, 483, 483, 483, 483, 483, 483, 483, 483, 483,
      483, 483, 483, 483, 483, 483, 483, 483, 483, 483,
      483, 483, 483, 483, 483, 483, 483, 483, 483, 483,
      483, 483, 483, 483, 483, 483, 483, 483, 483, 483,
      483, 483, 483, 483, 483, 483, 483, 483, 483, 483,
      483, 483, 483, 483, 483, 483, 483, 483, 483, 483,
      483, 483, 483, 483, 483, 483, 483, 483, 483, 483,
      483, 483, 483, 483, 483, 483, 483, 483, 483, 483,
      483, 483, 483, 483, 483, 483, 483, 483, 483, 483,
      483, 483, 483, 483, 483, 483, 483, 483, 483, 483,
      483, 483, 483, 483, 483, 483, 483, 483, 483, 483,
      483, 483, 483, 483, 483, 483
    };
  register int hval = len;

  switch (hval)
    {
      default:
      case 6:
        hval += asso_values[(unsigned char)str[5]];
      case 5:
        hval += asso_values[(unsigned char)str[4]];
      case 4:
        hval += asso_values[(unsigned char)str[3]];
      case 3:
        hval += asso_values[(unsigned char)str[2]];
      case 2:
        hval += asso_values[(unsigned char)str[1]];
      case 1:
        hval += asso_values[(unsigned char)str[0]];
        break;
    }
  return hval + asso_values[(unsigned char)str[len - 1]];
}

#ifdef __GNUC__
__inline
#endif
const char *
libc_name_p (str, len)
     register const char *str;
     register unsigned int len;
{
  enum
    {
      TOTAL_KEYWORDS = 156,
      MIN_WORD_LENGTH = 3,
      MAX_WORD_LENGTH = 10,
      MIN_HASH_VALUE = 3,
      MAX_HASH_VALUE = 482
    };

  static const char * const wordlist[] =
    {
      "cos",
      "feof",
      "free",
      "sqrt",
      "wcsrtombs",
      "pow",
      "towctrans",
      "wcsstr",
      "wcstombs",
      "strstr",
      "ferror",
      "wcsxfrm",
      "wcsftime",
      "exit",
      "exp",
      "modf",
      "strxfrm",
      "wmemset",
      "memset",
      "strftime",
      "frexp",
      "time",
      "ctime",
      "wcstod",
      "fwide",
      "wcscmp",
      "wmemmove",
      "strtod",
      "fmod",
      "wcschr",
      "wcsrchr",
      "strcmp",
      "wctype",
      "toupper",
      "towupper",
      "strchr",
      "strrchr",
      "wmemcmp",
      "iswctype",
      "gmtime",
      "difftime",
      "btowc",
      "iswprint",
      "iswxdigit",
      "cosh",
      "memcmp",
      "wmemchr",
      "isupper",
      "iswupper",
      "iswdigit",
      "memchr",
      "isxdigit",
      "wmemcpy",
      "mbtowc",
      "setbuf",
      "mbstowcs",
      "wcscpy",
      "memmove",
      "vswprintf",
      "acos",
      "mbrtowc",
      "wcrtomb",
      "mbsrtowcs",
      "atof",
      "strcpy",
      "setlocale",
      "wcscat",
      "isdigit",
      "log10",
      "tolower",
      "floor",
      "towlower",
      "strcat",
      "log",
      "mktime",
      "wcstoul",
      "fseek",
      "memcpy",
      "wcstok",
      "strtoul",
      "wcscspn",
      "islower",
      "div",
      "iswlower",
      "atexit",
      "strtok",
      "setvbuf",
      "strcspn",
      "isspace",
      "iswspace",
      "asctime",
      "wctob",
      "wcsncmp",
      "atoi",
      "ldexp",
      "strncmp",
      "wcspbrk",
      "wctomb",
      "swprintf",
      "sprintf",
      "strpbrk",
      "abs",
      "fabs",
      "wcsncpy",
      "ispunct",
      "iswpunct",
      "strncpy",
      "iswgraph",
      "isprint",
      "isgraph",
      "wcscoll",
      "wcstol",
      "vsprintf",
      "strcoll",
      "strtol",
      "sscanf",
      "clearerr",
      "swscanf",
      "sinh",
      "wcsncat",
      "getenv",
      "ceil",
      "clock",
      "wctrans",
      "strncat",
      "ldiv",
      "iswcntrl",
      "wcsspn",
      "iscntrl",
      "sin",
      "strspn",
      "mbsinit",
      "longjmp",
      "rand",
      "srand",
      "labs",
      "tanh",
      "calloc",
      "atol",
      "localtime",
      "realloc",
      "malloc",
      "atan2",
      "tan",
      "wcslen",
      "strlen",
      "iswalpha",
      "localeconv",
      "asin",
      "iswalnum",
      "isalnum",
      "isalpha",
      "mblen",
      "mbrlen",
      "atan",
      "signal"
    };

  static const short lookup[] =
    {
        -1,   -1,   -1,    0,    1,   -1,   -1,   -1,
        -1,   -1,   -1,    2,   -1,   -1,   -1,   -1,
        -1,    3,   -1,   -1,    4,   -1,    5,    6,
         7,   -1,   -1,   -1,   -1,    8,   -1,   -1,
        -1,    9,   10,   11,   -1,   -1,   12,   13,
        14,   -1,   15,   -1,   16,   17,   18,   19,
        -1,   20,   21,   22,   23,   24,   -1,   -1,
        -1,   -1,   25,   -1,   26,   27,   -1,   28,
        29,   30,   -1,   31,   32,   -1,   33,   -1,
        34,   35,   36,   -1,   37,   -1,   -1,   38,
        39,   -1,   -1,   -1,   40,   41,   -1,   -1,
        42,   43,   44,   45,   46,   47,   -1,   48,
        49,   50,   51,   -1,   52,   -1,   -1,   53,
        54,   55,   56,   57,   -1,   58,   59, -273,
        62,   63,   -1,   64,  -96,   -2,   65,   66,
        67,   -1,   68,   -1,   69,   70,   71,   -1,
        72,   -1,   -1,   73,   -1,   -1,   -1,   74,
        75,   76,   -1,   77,   -1,   -1,   78,   -1,
        -1,   79,   80,   81,   82,   83,   84,   85,
        -1,   -1,   86,   87,   88,   -1,   89,   90,
        -1,   91,   -1,   92,   -1,   93,   -1,   -1,
        -1,   94,   -1,   -1,   95,   -1,   96,   -1,
        -1,   -1,   -1,   97,   98,   99,   -1,  100,
        -1,  101,  102,  103,   -1,   -1,   -1,   -1,
        -1,  104,   -1,  105,  106,   -1,   -1,   -1,
        -1,   -1,  107,   -1,   -1,   -1,   -1,  108,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,  109,   -1,   -1,  110,   -1,  111,   -1,
        -1,   -1,   -1,   -1,  112,  113,   -1,  114,
        -1,  115,  116,  117,   -1,  118,  119,  120,
        -1,   -1,  121,  122,   -1,  123,   -1,  124,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,  125,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,  126,  127,   -1,   -1,   -1,  128,  129,
        -1,   -1,   -1,  130,  131,   -1,   -1,  132,
       133,  134,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,  135,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
       136,   -1,   -1,   -1,  137,  138,   -1,   -1,
        -1,   -1,  139,   -1,  140,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,  141,   -1,  142,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,  143,  144,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,  145,   -1,   -1,   -1,
        -1,   -1,  146,   -1,   -1,   -1,   -1,   -1,
        -1,  147,  148,   -1,   -1,   -1,   -1,   -1,
        -1,  149,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,  150,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,  151,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,  152,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,  153,   -1,
        -1,   -1,   -1,   -1,   -1,  154,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,  155
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register int index = lookup[key];

          if (index >= 0)
            {
              register const char *s = wordlist[index];

              if (*str == *s && !strcmp (str + 1, s + 1))
                return s;
            }
          else if (index < -TOTAL_KEYWORDS)
            {
              register int offset = - 1 - TOTAL_KEYWORDS - index;
              register const char * const *wordptr = &wordlist[TOTAL_KEYWORDS + lookup[offset]];
              register const char * const *wordendptr = wordptr + -lookup[offset + 1];

              while (wordptr < wordendptr)
                {
                  register const char *s = *wordptr;

                  if (*str == *s && !strcmp (str + 1, s + 1))
                    return s;
                  wordptr++;
                }
            }
        }
    }
  return 0;
}
