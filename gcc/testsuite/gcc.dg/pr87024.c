/* { dg-do compile } */
/* { dg-options "-O -fno-tree-dce" } */

static inline void __attribute__((always_inline))
mp ()
{
  (void) __builtin_va_arg_pack_len ();
}

void
ui (void)
{
  mp ();
}
