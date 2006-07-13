/* Test that -fvisibility-inlines-hidden doesn't affect static variables. */
/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-options "-fvisibility-inlines-hidden" } */
/* { dg-final { scan-not-hidden "_ZZN3foo7my_funcEvE1x" } } */

struct foo 
{
  int my_func() {
    static int x;
    return x++;
  }
};

int t() 
{
  foo f;
  return f.my_func();
}
