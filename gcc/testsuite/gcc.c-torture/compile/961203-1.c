/* The structure is too large for the xstormy16 - won't fit in 16
   bits.  */
/* { dg-do assemble { xfail xstormy16-*-* h8300-*-* m6811-*-* m6812-*-* } } */

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
    exit (1);
}
