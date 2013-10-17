/* PR target/42894 */
/* { dg-do compile } */
/* { dg-require-effective-target tls } */

extern __thread int t;

void
foo (int a)
{
  t = a;
}
