/* Reduced from ICE seen with coreutils-9.1:lib/human.c, which is GPLv3+. */

/* { dg-additional-options "-Wno-analyzer-too-complex" } */
/* { dg-additional-options "-Wno-analyzer-symbol-too-complex" } */

typedef __SIZE_TYPE__ size_t;

char *
group_number (char *number, size_t numberlen, char const *grouping,
              char const *thousands_sep)
{
  char *d;
  size_t grouplen = (18446744073709551615UL);
  size_t thousands_seplen = __builtin_strlen (thousands_sep);
  size_t i = numberlen;

  char buf[100];
  __builtin_memcpy (buf, number, numberlen);
  d = number + numberlen;

  for (;;)
    {
      unsigned char g = *grouping;

      if (g)
        {
          grouplen = g < 0x7f ? g : i;
          grouping++;
        }

      if (i < grouplen)
        grouplen = i;

      d -= grouplen;
      i -= grouplen;
      __builtin_memcpy (d, buf + i, grouplen);
      if (i == 0)
        return d;

      d -= thousands_seplen;
      __builtin_memcpy (d, thousands_sep, thousands_seplen);
    }
}
