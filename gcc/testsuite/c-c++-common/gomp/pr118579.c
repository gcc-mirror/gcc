/* { dg-do compile } */

/* Make sure errors in variable-lists are diagnosed in the right place.  */

void fvar(int *, int *);
#pragma omp declare variant(fvar) \
  match(construct={dispatch}) \
  adjust_args(need_device_ptr: yyy, xxx, xxx)
/* { dg-error "37: .xxx. is specified more than once" "" { target *-*-* } .-1 } */
void f(int *xxx, int*yyy);


extern void frobnicate (int);
void g (int x, int y)
{
  int l = x + y;
  static int s = 42;
  frobnicate (s);
#pragma omp threadprivate (l, s)
/* { dg-error "28: automatic variable .l. cannot be .threadprivate." "" { target *-*-* } .-1 } */
/* { dg-error "31: .s. declared .threadprivate. after first use" "" { target *-*-* } .-2 } */
  {
    f (&l, &s);
  }
}
