/* { dg-do run } */
/* { dg-options "-O3 -std=gnu++11" } */

using complex_t = int __complex__;

struct A {
    complex_t value;
    A(double r) : value{0, r} {}
};

[[gnu::noipa]]
void f(int a)
{
  if (a != 1)
    __builtin_abort();
}
[[gnu::noipa]] void g(const char *, int x){}


void test(const complex_t &c, const int &x) {
    if (x < 0)
        g("%d\n", x);
    else
    {
        f( __imag__ c);
    }
}

void f1() {
    {
        A a{1};
        test(a.value, 123);
    }
}

int main()
{
        f1();
	return 0;
}
