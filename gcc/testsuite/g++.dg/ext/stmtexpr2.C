// { dg-do run }
// { dg-options "" }

extern "C" int printf (char const *, ...);
extern "C" void abort ();

// There are two alternate legal renderings.
static unsigned int alt1[] = { 11, 10, 21, 110, 111, 121 };
static unsigned int alt2[] = { 10, 11, 21, 111, 110, 121 };

static unsigned int pointer = 0;
static unsigned int *which;

static void Check (unsigned t, unsigned i, void const *ptr, char const *name)
{
  printf ("%d %d %p %s\n", t, i, ptr, name);

  if (pointer > sizeof(alt1)/sizeof(alt1[0]))
    abort ();
  if (pointer == 0)
    {
      if (t + i == alt1[0])
	which = &alt1[0];
      else if (t + i == alt2[0])
	which = &alt2[0];
      else
	abort ();
    }
  else if (t + i != which[pointer])
    abort ();
  pointer++;
}

struct A 
{
  int I;

  A (int i) : I(i) { Check (0, I, this, __PRETTY_FUNCTION__); }
  ~A () { Check (100, I, this, __PRETTY_FUNCTION__); }
  A (A const &a) : I(a.I) { Check (200, I, this, __PRETTY_FUNCTION__); }
  A &operator= (A const &a)
  { I = a.I; Check (300, I, this, __PRETTY_FUNCTION__); return *this; }
  void Foo () const { Check (400, I, this, __PRETTY_FUNCTION__); }
  A operator+ (A const &a) const
  { return A(I + a.I); }
};

int main ()
{
  ({ A(10) + A(11); });
}
