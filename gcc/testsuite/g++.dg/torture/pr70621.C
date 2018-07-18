float foo();

struct A
{
  static float x;  // { dg-message "previous declaration" }
};

double A::x = foo();  // { dg-error "conflicting declaration" }

void bar()
{
  A::x = 0;
}
