/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O2 -fpic -mcmodel=large" } */

__extension__ typedef __SIZE_TYPE__ size_t;
typedef void (*func_ptr) (void);

static func_ptr __DTOR_LIST__[1] = { (func_ptr) (-1) };

void
__do_global_dtors_aux (void)
{
  extern func_ptr __DTOR_END__[];
  size_t dtor_idx = 0;
  const size_t max_idx = __DTOR_END__ - __DTOR_LIST__ - 1;
  func_ptr f;

  while (dtor_idx < max_idx)
    {
      f = __DTOR_LIST__[++dtor_idx];
      f ();
    }
}
