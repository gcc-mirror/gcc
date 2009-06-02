// { dg-options "-std=c++0x" }

void f(double);
int main()
{
  f({{1}});			// { dg-error "too many braces" }
  // { dg-error "" "" { target *-*-* } 6 } allow other errors, too
}
