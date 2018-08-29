/* { dg-do compile } */
/* { dg-skip-if "" { ! { clmcpu } } } */
/* { dg-options "-mcpu=archs -g -O1 -fpic -mlra" } */

/* Check if delegitimize address returns correctly the un-obfuscated
   address.  */

typedef struct {
  long long tv_usec;
} t_a;

static t_a a;

int b;
extern void fn2 (t_a);

void fn1 (void)
{
 again:
  fn2(a);
  if (b)
    goto again;
}
