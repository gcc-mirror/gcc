struct Base {
        Base();
  Base(const Base &);
  Base & operator = (const Base &);
};

struct Derived : public Base {};

Derived derived();
const Base &b = derived();
