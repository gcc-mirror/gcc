// { dg-do run }

int c;

struct Base {
  Base() {}
  Base(const Base &) { ++c; }
  Base & operator = (const Base &);
};

struct Derived : public Base {};

const Base &b = Derived();

int main()
{
  return c;  // No copies should be required.
}
