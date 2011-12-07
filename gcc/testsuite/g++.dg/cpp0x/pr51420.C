// { dg-options "-std=c++11" }

void
foo()
{
  float x = operator"" _F();  //  { dg-error  "was not declared in this scope" }
  float y = 0_F;  //  { dg-error  "unable to find numeric literal operator" }
}
