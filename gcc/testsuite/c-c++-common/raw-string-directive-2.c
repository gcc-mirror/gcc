/* { dg-do run } */
/* { dg-options "-std=gnu99" { target c } } */
/* { dg-options "-std=c++11" { target c++ } } */

#define S1 R"(three
line
string)"

#define S2 R"(pasted
two line)" " string"

#define X(a, b) a b R"(
one more)"

const char *s1 = S1;
const char *s2 = S2;
const char *s3 = X(S1, R"(
with this line plus)");

int main ()
{
  const char s1_correct[] = "three\nline\nstring";
  if (__builtin_memcmp (s1, s1_correct, sizeof s1_correct))
    __builtin_abort ();

  const char s2_correct[] = "pasted\ntwo line string";
  if (__builtin_memcmp (s2, s2_correct, sizeof s2_correct))
    __builtin_abort ();

  const char s3_correct[] = "three\nline\nstring\nwith this line plus\none more";
  if (__builtin_memcmp (s3, s3_correct, sizeof s3_correct))
    __builtin_abort ();
}
