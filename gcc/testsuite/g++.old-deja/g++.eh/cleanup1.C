// Bug: obj gets destroyed twice because the fixups for the return are
// inside its cleanup region.

extern "C" int printf (const char *, ...);

int d;

struct myExc { };

struct myExcRaiser {
  ~myExcRaiser() { throw myExc(); }
};

struct stackObj {
  ~stackObj() { ++d; printf ("stackObj::~stackObj()\n"); };
};

int test()
{
  myExcRaiser rais;
  stackObj obj;
  return 0;
}

int main()
{
  try {
    test();
  }
  catch (myExc &) {
    return d != 1;
  }
  return 1;
}
