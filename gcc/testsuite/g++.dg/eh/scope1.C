// Test that we've scoped the destructor properly for variables declared
// in a conditional.
// { dg-do run }

extern "C" void abort ();

class C
{
  bool live;
 public:
  C();
  C(const C &);
  ~C ();
  operator bool() const;
};

void f1 ()
{
  while (C br = C()) abort ();
}

void f2 ()
{
  for (; C br = C(); ) abort ();
}

void f3 ()
{
  if (C br = C()) abort ();
}

void f4 ()
{
  switch (C br = C())
    {
    default:
      abort ();
    case false:
      break;
    }
}

int main()
{
  f1(); f2(); f3(); f4();
  return 0;
}

C::C()
{
  live = true;
}

C::C(const C &o)
{
  if (!o.live)
    abort ();
  live = true;
}

C::~C()
{
  live = false;
}

C::operator bool() const
{
  if (!live)
    abort ();
  return false;
}
