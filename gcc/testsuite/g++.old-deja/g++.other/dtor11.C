// Origin: Mark Mitchell <mark@codesourcery.com>

extern "C" void abort ();

int j;

struct S {
  S () { ++j; }
  S (const S&) { ++j; }
  ~S () {
    if (--j < 0)
      abort ();
   }
};

struct T {
  void g (S) {
  };
};

struct U {
  int i;
  S s;
};

U u;

U f () { return u; }

int main ()
{
  T t;
  t.g (f ().s);
}
