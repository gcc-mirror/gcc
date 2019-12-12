// { dg-do assemble  }
struct A {
  A();
  A(A);				// { dg-error "3:invalid constructor" } copy ctor must take reference
};
int main()
{
  A a;
  A b(a);			// causes compiler segfault
}
