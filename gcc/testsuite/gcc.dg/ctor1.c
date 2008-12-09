/* Related to PR c++/38410.
   We shouldn't write out a static variable for an all-zero aggregate
   initializer.  The variable named C.0 was created by
   gimplify_init_constructor. */
/* { dg-final { scan-assembler-not "C\\.0" } } */

int main()
{
  int a[] = { 0,0 };
}
