/* PR71633 */
/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && { ! x32 } } } } */
/* { dg-options "-Werror=chkp -mmpx -fcheck-pointer-bounds -O1 -fchkp-flexible-struct-trailing-arrays" } */

struct a
{
  struct
  {
    int e[1];
  } f;
};

int g(a *ptr)
{
  return ptr->f.e[1];
}
