// GROUPS passed copy-ctors
extern "C" int printf (const char *, ...);
extern "C" void exit (int);

void die () { printf ("FAIL\n"); exit (1); }

class B {
public:
  B() {}
  B(const B &) { printf ("PASS\n"); exit (0); };
private:
    int x;
};

class A : public B {
public:
    A() {}

  A(const B &) { printf ("FAIL\n"); exit (1); }
};

int
main()
{
    A a;
    A b(a);

    printf ("FAIL\n");
    return 1;
}
