/* Reduced from haproxy-2.7.1's cfgparse.c.  */

typedef __SIZE_TYPE__ size_t;

extern int
strcmp(const char* __s1, const char* __s2)
  __attribute__((__nothrow__, __leaf__)) __attribute__((__pure__))
  __attribute__((__nonnull__(1, 2)));

extern int
strncmp(const char* __s1, const char* __s2, size_t __n)
  __attribute__((__nothrow__, __leaf__)) __attribute__((__pure__))
  __attribute__((__nonnull__(1, 2)));

enum
{
 /* [...snip...] */
  _ISdigit = ((3) < 8 ? ((1 << (3)) << 8) : ((1 << (3)) >> 8)),
 /* [...snip...] */
};

extern const unsigned short int**
__ctype_b_loc(void) __attribute__((__nothrow__, __leaf__))
  __attribute__((__const__));

unsigned int str2uic(const char* s);

char*
memprintf(char** out, const char* format, ...)
  __attribute__((format(printf, 2, 3)));

int
parse_process_number(const char* arg,
                     unsigned long* proc,
                     int max,
                     int* autoinc,
                     char** err)
{
  if (autoinc) {
    *autoinc = 0;
    if (strncmp(arg, "auto:", 5) == 0) {
      arg += 5;
      *autoinc = 1;
    }
  }

  if (strcmp(arg, "all") == 0) /* { dg-bogus "pointer 'dash' is dereferenced here" } */
    *proc |= ~0UL;
  else if (strcmp(arg, "odd") == 0)
    *proc |= ~0UL / 3UL;
  else if (strcmp(arg, "even") == 0)
    *proc |= (~0UL / 3UL) << 1;
  else {
    const char *p, *dash = ((void*)0);
    unsigned int low, high;

    for (p = arg; *p; p++) {
      if (*p == '-' && !dash) /* { dg-bogus "check of 'dash' for NULL after already dereferencing it" } */
        dash = p;
      else if (!((*__ctype_b_loc())[(int)(((unsigned char)*p))] &
                 (unsigned short int)_ISdigit)) {
        memprintf(err, "'%s' is not a valid number/range.", arg);
        return -1;
      }
    }

    low = high = str2uic(arg);
    if (dash) /* { dg-bogus "check of 'dash' for NULL after already dereferencing it" } */
      high = ((!*(dash + 1)) ? max : str2uic(dash + 1));

    if (high < low) {
      unsigned int swap = low;
      low = high;
      high = swap;
    }

    if (low < 1 || low > max || high > max) {
      memprintf(err,
                "'%s' is not a valid number/range."
                " It supports numbers from 1 to %d.\n",
                arg,
                max);
      return 1;
    }

    for (; low <= high; low++)
      *proc |= 1UL << (low - 1);
  }
  *proc &= ~0UL >> (((unsigned int)sizeof(long) * 8) - max);

  return 0;
}
