/* PR preprocessor/103415 */
/* { dg-do run } */
/* { dg-options "-std=gnu99" { target c } } */
/* { dg-options "-std=c++20" { target c++ } } */

#define n(x, ...) = #__VA_OPT__(x##3)
#define o(x, ...) #__VA_OPT__(x##__VA_ARGS__##9)
const char *c n(1 2, 4);
const char *d = o(5  6, 7	8);

int
main ()
{
  if (__builtin_strcmp (c, "1 23")
      || __builtin_strcmp (d, "5 67 89"))
    __builtin_abort ();
  return 0;
}
