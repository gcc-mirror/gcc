// InterViews (ibuild) requires this to work.

extern "C" void exit(int);

void *old;

class c1
{
  int i;
public:
  c1 () {}
};

class c2
{
  int j;
public:
  c2 () {}
};

class c3 : public c1, public c2
{
public:
  c3 ()
    {
      old = this;
      // printf("new object c3 at %x\n", (int)this); }
    }
};

c2* f ()
{
  c2* n = new c3 ();
  // printf ("new object c3 casted to c2 at %x\n", (int)n);
  return n;
}

int main ()
{
  c3* o = (c3*)f ();
  // printf("new object c3, upcasted from c2 at %x\n", (int)o);
  // if old and o are not the same, fail the test case.
  if (old != o)
    exit(1);
  return 0;
}
