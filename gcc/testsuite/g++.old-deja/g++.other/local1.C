// Build don't run:

struct Outer {
  virtual ~Outer() {}
};

int
main()
{
  { struct Inner : virtual public Outer {} inner; }
  { struct Inner : virtual public Outer {} inner; }
}

