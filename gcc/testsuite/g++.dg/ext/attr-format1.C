// PR c++/47634
// { dg-do compile }

class Base {
public:
  Base() { }
};

class VDerived : public virtual Base {
public:
  VDerived(int x, const char * f, ...) __attribute__((format(printf, 3, 4)));
};

class Derived : public Base {
public:
  Derived(int x, const char * f, ...) __attribute__((format(printf, 3, 4)));
};

VDerived::VDerived(int, const char *, ...)
{
}

Derived::Derived(int, const char *, ...)
{
}

int
main(int, char **)
{
  throw VDerived(1, "%s %d", "foo", 1);
  throw Derived(1, "%s %d", "bar", 1);
}
