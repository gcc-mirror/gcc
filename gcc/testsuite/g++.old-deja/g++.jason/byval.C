// Bug: a is destroyed in both foo() and main()

int count;

struct A {
  double a,b;
  A(int) { count++; }
  A(const A&) { count++; }
  ~A() { count--; }
};

void foo (A a)
{ }

int main()
{
  foo (1);
  return count;
}
