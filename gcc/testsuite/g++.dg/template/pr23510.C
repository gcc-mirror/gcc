// { dg-do compile } 
// { dg-options "-ftemplate-depth-15" }
template<unsigned int nFactor>
struct Factorial
{
  enum { nValue = nFactor * Factorial<nFactor - 1>::nValue }; // { dg-error "depth" } 
};

template<>
struct Factorial<0>
{
  enum { nValue = 1 };
};

static const unsigned int FACTOR = 20;

int main()
{
  Factorial<FACTOR>::nValue;  // { dg-message "from here" }
  return 0;
}

// { dg-prune-output "compilation terminated" }
