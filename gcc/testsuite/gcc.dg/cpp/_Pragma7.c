/*
  Origin: PR preprocessor/53469
  { dg-do compile }
 */

#define STRINGIFY(x) #x
#define TEST(x) \
  _Pragma(STRINGIFY(GCC diagnostic ignored "-Wunused-local-typedefs")) \
  typedef int myint;

void bar ()
{
  TEST(myint)
}
