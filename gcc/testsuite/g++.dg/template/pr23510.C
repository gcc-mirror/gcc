// { dg-do compile } 
// { dg-options "-ftemplate-depth-15" }
template<unsigned int nFactor>
struct Factorial
{
  enum { nValue = nFactor * Factorial<nFactor - 1>::nValue }; // { dg-error "depth exceeds maximum" "exceeds" } 
  // { dg-message "recursively required" "recurse" { target *-*-* } 6 }
  // { dg-error "incomplete type" "incomplete" { target *-*-* } 6 } 
} // { dg-error "expected ';' after" }

  template<>
  struct Factorial<0>
  {
    enum { nValue = 1 };
  };

    static const unsigned int FACTOR = 20;

int main()
{
  Factorial<FACTOR>::nValue;
  return 0;
}
