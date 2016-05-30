// { dg-do compile { target c++11 } }

void
foo()
{
  float x = operator"" _F();  //  { dg-error  "13:'operator\"\"_F' was not declared in this scope" }
  float y = 0_F;  //  { dg-error  "unable to find numeric literal operator" }
}
