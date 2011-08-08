/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef void (*func_ptr) (void);
static func_ptr __CTOR_END__[1] = { (func_ptr) 0 };
static void __attribute__((used))
__do_global_ctors_aux (void)
{
  func_ptr *p;
  for (p = __CTOR_END__ - 1; *p != (func_ptr) -1; p--)
    (*p) ();
}
