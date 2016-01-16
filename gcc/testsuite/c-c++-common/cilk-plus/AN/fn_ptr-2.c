/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

typedef void (*f) (void *);
f b[1024];
void *c[1024][1024];

int
main (void)
{
  (b[:]) (c[:][:]); /* { dg-error "rank mismatch" "" { xfail *-*-* } } */
  return 0;
}

