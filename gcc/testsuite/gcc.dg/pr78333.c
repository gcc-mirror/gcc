/* { dg-do link } */
/* { dg-options "-finstrument-functions" } */

extern inline __attribute__((gnu_inline, always_inline)) int foo () { }
int main()
{
  foo ();
  return 0;
}
