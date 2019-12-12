/* PR middle-end/88074 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-add-options float128 } */
/* { dg-require-effective-target float128 } */
/* { dg-final { scan-tree-dump-not "link_error " "optimized" } } */

extern void link_error (void);
int
main ()
{
  if (((__FLT128_MAX__ * 0.5 + __FLT128_MAX__ * 0.5i)
       / (__FLT128_MAX__ * 0.25 + __FLT128_MAX__ * 0.25i))
      != (_Complex _Float128) 2)
    link_error ();
  return 0;
}
