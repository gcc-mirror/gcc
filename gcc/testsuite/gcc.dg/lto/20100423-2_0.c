/* { dg-lto-do link } */
/* { dg-lto-options {{-O2 -flto} {-O2 -fwhopr} {-O3 -flto} {-O3 -fwhopr}} } */

#include <stdio.h>

typedef unsigned char uch;
extern uch inbuf[];
unsigned insize;
char *progname;
extern void read_error (void);
int fill_inbuf(int eof_ok)
{
  if (insize == 0) 
    {
      if (eof_ok)
        return -1;
      read_error();
    }
  return inbuf[0];
}
void read_error(void)
{
  fprintf(stderr, "\n%s: ", progname);
}

