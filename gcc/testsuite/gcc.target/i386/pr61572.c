/* { dg-do compile } */
/* { dg-options "-O2" } */

struct autofs_sb_info
{
  int exp_timeout;
};
void *f;
int g;
static int fn1 (struct autofs_sb_info *p1)
{
  int a, b;
  a = (
       {
       register __typeof__(0) c
#if defined __x86_64__
       asm("rdx")
#endif
       ;
       b = c;
       int d;
       __typeof__(0) e;
       e = p1->exp_timeout / 1000;
       switch (0)
       default:
       asm("" : "=a"(d) : "0"(e), ""(0));
       d;
       });
  if (a)
    return 1;
  if (b)
    p1->exp_timeout = 0;
  return 0;
}

int fn2 ()
{
  struct autofs_sb_info *h = f;
  switch (g)
    {
      case 0 ?:
0 : return fn1 (h);
      default:
    return 0;
    }
}
