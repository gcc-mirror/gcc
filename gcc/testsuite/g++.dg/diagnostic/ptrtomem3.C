// Check that the diagnostic for a pointer-to-member expression has the caret
// at the &.

struct A
{
  int i;
};

void f();

int main()
{
  return &A::i; 		// { dg-error "10:cannot convert" }
}
