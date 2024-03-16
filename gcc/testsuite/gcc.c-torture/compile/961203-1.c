/* The structure is too large for the xstormy16 - won't fit in 16
   bits.  */
/* { dg-do assemble } */
/* { dg-skip-if "Array too big" { "pdp11-*-*" } { "-mint32" } } */

#if __INT_MAX__ >= 2147483647L
struct s {
  char a[0x32100000];
  int x:30, y:30;
};

int
main ()
{
  struct s* p;

  p = (struct s*) 0;
  if (p->x == p->y)
    __builtin_exit (1);
}
#else
int g;
#endif
