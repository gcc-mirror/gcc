/* Test for cross x86_64<->w64 abi va_list calls.
*/
/* Origin: Kai Tietz <kai.tietz@onevision.com> */
/* { dg-do run } */
/* { dg-options "-std=gnu99" } */
#include "callabi.h"

extern __SIZE_TYPE__ strlen (const char *);
extern int sprintf (char *,const char *, ...);
extern void abort (void);

static
void CALLABI_CROSS vdo_cpy (char *s, CROSS_VA_LIST argp)
{
  __SIZE_TYPE__ len;
  char *r = s;
  char *e;
  *r = 0;
  for (;;) {
    e = CROSS_VA_ARG (argp,char *);
    if (*e == 0) break;
    sprintf (r,"%s", e);
    r += strlen (r);
  }
}

static
void CALLABI_CROSS do_cpy (char *s, ...)
{
  CROSS_VA_LIST argp;
  CROSS_VA_START (argp, s);
  vdo_cpy (s, argp);
  CROSS_VA_END (argp);
}

int main ()
{
  char s[256];

  do_cpy (s, "1","2","3","4", "5", "6", "7", "");

  if (s[0] != '1' || s[1] !='2' || s[2] != '3' || s[3] != '4'
      || s[4] != '5' || s[5] != '6' || s[6] != '7' || s[7] != 0)
    abort ();

  return 0;
}
