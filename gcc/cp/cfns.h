/* C code produced by gperf version 2.7 */
/* Command-line: gperf -o -C -E -k 1-6,$ -j1 -D -N libc_name_p /home/jason/eg/gcc/cp/cfns.gperf  */
/* maximum key range = 1020, duplicates = 1 */

#ifdef __GNUC__
__inline
#endif
static unsigned int
hash (str, len)
     register const char *str;
     register unsigned int len;
{
  static const unsigned short asso_values[] =
    {
      1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038,
      1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038,
      1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038,
      1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038,
      1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038,    0,    1,
         0, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038,
      1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038,
      1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038,
      1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038,
      1038, 1038, 1038, 1038, 1038, 1038, 1038,  247,  218,  144,
         0,    0,   40,    7,  126,  184,    2,   15,  146,   67,
         9,   60,    0,    0,    3,    0,    7,    8,  197,    1,
        40,    8, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038,
      1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038,
      1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038,
      1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038,
      1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038,
      1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038,
      1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038,
      1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038,
      1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038,
      1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038,
      1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038,
      1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038,
      1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038, 1038,
      1038, 1038, 1038, 1038, 1038, 1038
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
      TOTAL_KEYWORDS = 207,
      MIN_WORD_LENGTH = 3,
      MAX_WORD_LENGTH = 10,
      MIN_HASH_VALUE = 18,
      MAX_HASH_VALUE = 1037
    };

  static const char * const wordlist[] =
    {
      "gets",
      "puts",
      "sqrt",
      "strerror",
      "strstr",
      "strspn",
      "exp",
      "free",
      "fgets",
      "fputs",
      "fgetws",
      "fputws",
      "pow",
      "fseek",
      "perror",
      "strtod",
      "toupper",
      "towupper",
      "frexp",
      "strtok",
      "fsetpos",
      "ferror",
      "freopen",
      "fgetpos",
      "fopen",
      "wmemset",
      "memset",
      "system",
      "wcsstr",
      "wctype",
      "strxfrm",
      "wcsspn",
      "strcspn",
      "fmod",
      "strcpy",
      "strncpy",
      "strlen",
      "ungetwc",
      "feof",
      "ldexp",
      "isupper",
      "rewind",
      "iswupper",
      "sin",
      "cos",
      "modf",
      "iswpunct",
      "wcstod",
      "log10",
      "log",
      "wcsrtombs",
      "strcmp",
      "fwide",
      "towctrans",
      "strncmp",
      "strtoul",
      "fwrite",
      "exit",
      "swprintf",
      "wcstok",
      "strftime",
      "sprintf",
      "wprintf",
      "strpbrk",
      "time",
      "rand",
      "srand",
      "wmemmove",
      "tan",
      "tolower",
      "fwprintf",
      "towlower",
      "wcstombs",
      "printf",
      "fprintf",
      "strchr",
      "strrchr",
      "wmemcpy",
      "fread",
      "getwchar",
      "putwchar",
      "longjmp",
      "memcpy",
      "wcsxfrm",
      "wcscspn",
      "getc",
      "putc",
      "getwc",
      "putwc",
      "wcscpy",
      "wcsncpy",
      "wcslen",
      "floor",
      "setbuf",
      "ungetc",
      "rename",
      "remove",
      "gmtime",
      "mktime",
      "fgetc",
      "fputc",
      "fgetwc",
      "fputwc",
      "memcmp",
      "iswctype",
      "wmemcmp",
      "ispunct",
      "mbstowcs",
      "wcscmp",
      "mbsrtowcs",
      "setlocale",
      "wcsncmp",
      "wcstoul",
      "strtol",
      "wcsftime",
      "iswprint",
      "wcspbrk",
      "iswdigit",
      "isprint",
      "fclose",
      "atof",
      "islower",
      "iswlower",
      "ctime",
      "wmemchr",
      "memchr",
      "wctrans",
      "strcat",
      "getenv",
      "strncat",
      "iswxdigit",
      "wcschr",
      "wcsrchr",
      "isxdigit",
      "vswprintf",
      "raise",
      "iswspace",
      "vsprintf",
      "vwprintf",
      "vprintf",
      "swscanf",
      "sinh",
      "tmpfile",
      "asin",
      "mblen",
      "acos",
      "mbrlen",
      "cosh",
      "difftime",
      "memmove",
      "abs",
      "tmpnam",
      "vfwprintf",
      "setvbuf",
      "vfprintf",
      "scanf",
      "sscanf",
      "wscanf",
      "fwscanf",
      "ftell",
      "fflush",
      "atexit",
      "iswcntrl",
      "iscntrl",
      "mbrtowc",
      "wcrtomb",
      "fabs",
      "wcstol",
      "strcoll",
      "atan2",
      "tanh",
      "atan",
      "fscanf",
      "clock",
      "getchar",
      "putchar",
      "abort",
      "clearerr",
      "wcscat",
      "wcsncat",
      "isdigit",
      "isgraph",
      "iswgraph",
      "btowc",
      "div",
      "isspace",
      "atol",
      "labs",
      "ceil",
      "mbtowc",
      "wcscoll",
      "wctob",
      "asctime",
      "iswalnum",
      "isalnum",
      "mbsinit",
      "atoi",
      "wctomb",
      "ldiv",
      "signal",
      "realloc",
      "localtime",
      "iswalpha",
      "localeconv",
      "isalpha",
      "malloc",
      "calloc"
    };

  static const short lookup[] =
    {
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,    0,    1,   -1,    2,   -1,   -1,
        -1,   -1,   -1,    3,   -1,    4,   -1,   -1,
        -1,   -1,    5,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,    6,   -1,   -1,   -1,    7,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,    8,    9,   10,   11,   -1,
        -1,   12,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   13,   -1,   -1,   14,   -1,
        -1,   -1,   -1,   15,   -1,   16,   -1,   17,
        18,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   19,   20,   -1,   -1,   -1,   21,   22,
        -1,   23,   -1,   24,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   25,   -1,   -1,
        -1,   -1,   26,   27,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   28,   -1,   29,   30,
        -1,   31,   32,   33,   -1,   -1,   -1,   -1,
        34,   -1,   35,   -1,   36,   -1,   -1,   37,
        38,   -1,   -1,   -1,   -1,   -1,   -1,   39,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   40,   41,   42,   43,   -1,   44,
        -1,   -1,   -1,   45,   -1,   -1,   -1,   -1,
        -1,   46,   47,   48,   -1,   -1,   -1,   49,
        50,   -1,   -1,   51,   -1,   -1,   52,   53,
        -1,   -1,   -1,   -1,   -1,   54,   55,   -1,
        -1,   56,   57,   -1,   -1,   58,   -1,   -1,
        59,   60,   61,   62,   -1,   63,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   64,   65,
        66,   -1,   -1,   -1,   -1,   -1,   67,   -1,
        -1,   -1,   -1,   68,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   69,   70,   71,   72,
        -1,   73,   74,   -1,   75,   76,   77,   78,
        79,   80,   81,   -1,   82,   -1,   83,   -1,
        -1,   84,   85,   86,   87,   88,   -1,   89,
        -1,   90,   -1,   91,   -1,   92,   -1,   93,
        -1,   -1,   -1,   -1,   -1,   94,   -1,   -1,
        -1,   -1,   -1,   -1,   95,   96,   -1,   -1,
        -1,   -1,   97,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   98,   99,  100,  101,  102,  103,
       104,  105,   -1,   -1,   -1,   -1,   -1,  106,
        -1,  107,  108,   -1,  109,   -1,  110,   -1,
        -1,   -1,   -1,   -1,  111,  112,   -1,  113,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
       114,   -1,   -1,  115,  116,   -1,   -1,  117,
        -1,   -1,  118,   -1,  119,   -1,  120,   -1,
        -1,  121,   -1,  122,   -1,   -1,   -1,  123,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,  124,
       125,   -1,  126,   -1,   -1,  127,   -1,  128,
       129,  130,   -1,  131,  132,   -1,  133,   -1,
        -1,   -1,  134,   -1,   -1,   -1,   -1,  135,
       136,  137,  138,   -1,   -1,   -1,   -1,  139,
       140,  141,   -1,  142,   -1,  143,  144,  145,
        -1,   -1,  146,   -1,  147,   -1,   -1,  148,
        -1,  149,   -1,   -1,  150,   -1,  151,   -1,
        -1,   -1,  152,   -1,   -1,  153,   -1,   -1,
        -1,  154,   -1,   -1,   -1,  155,  156,  157,
       158,   -1,  159,   -1,  160,   -1,   -1,   -1,
        -1,   -1,  161,  162,  163,   -1,   -1,   -1,
        -1,   -1,   -1, -719,   -1,  166,  167,  -43,
        -2,  168,   -1,  169,   -1,   -1,   -1,  170,
        -1,   -1,   -1,  171,   -1,   -1,  172,   -1,
        -1,  173,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,  174,  175,   -1,
        -1,   -1,   -1,  176,   -1,   -1,   -1,  177,
        -1,   -1,   -1,   -1,  178,   -1,   -1,  179,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,  180,  181,   -1,
       182,   -1,   -1,  183,   -1,  184,  185,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,  186,   -1,   -1,   -1,   -1,  187,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
       188,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,  189,
       190,   -1,   -1,   -1,   -1,  191,   -1,   -1,
       192,   -1,   -1,   -1,   -1,   -1,  193,   -1,
        -1,   -1,   -1,   -1,  194,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,  195,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,  196,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,  197,   -1,   -1,   -1,   -1,   -1,   -1,
       198,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,  199,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,  200,   -1,   -1,   -1,   -1,   -1,  201,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,  202,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,  203,   -1,
        -1,   -1,   -1,   -1,   -1,  204,   -1,   -1,
       205,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,  206
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
