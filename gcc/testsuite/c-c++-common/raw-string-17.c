/* PR preprocessor/57824 */
/* { dg-do run } */
/* { dg-options "-std=gnu99" { target c } } */
/* { dg-options "-std=c++11" { target c++ } } */

#define S(s) s
#define T(s) s "\n"

const char x[] = R"(
abc
)";
const char y[] = S(R"(
abc
)");
const char z[] = "\nabc\n";
const char w[] = T(R"(
abc)");

int
main ()
{
  if (sizeof x != sizeof y
      || sizeof x != sizeof z
      || sizeof x != sizeof w
      || __builtin_memcmp (x, y, sizeof x)
      || __builtin_memcmp (x, z, sizeof x)
      || __builtin_memcmp (x, w, sizeof x))
    __builtin_abort ();
  return 0;
}
