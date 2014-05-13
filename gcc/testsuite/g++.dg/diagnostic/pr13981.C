// PR c++/13981

struct A {};
struct B;  // { dg-message "is incomplete" }

void func( A *a );

int main()
{
  B *b = 0;
  func(b);  // { dg-error "cannot convert" }
}
