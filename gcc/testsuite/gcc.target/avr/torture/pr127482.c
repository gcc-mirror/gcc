/* { dg-do run } */
/* { dg-additional-options "-mcall-prologues" } */

#include <stdarg.h>
#include <stdlib.h>

#define NI __attribute((noipa))

NI void xx_fun (void) {}
NI void xx_vprintf (const char *fmt, va_list args)
{
  int i = va_arg (args, int);
  if (i != 0x1234)
    exit (10);
}

void error (const char *msg, ...)
{
  va_list args;
  xx_fun ();
  va_start (args, msg);
  xx_vprintf (msg, args);
  va_end (args);
  exit (0);
}

int main (void)
{
  error ("bad %d\n", 0x1234);
  return 0;
}
