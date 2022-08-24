/* { dg-do compile } */
/* { dg-options "-Og -fnon-call-exceptions -fsignaling-nans -fharden-compares" } */

struct S {
  S(float);
  S();
  operator float();
  ~S() {}
};

int
main() {
  S s_arr[] = {2};
  S var1;
  if (var1)
    ;
}
