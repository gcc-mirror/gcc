// Origin: Mark Mitchell <mark@codesourcery.com>

extern "C" void abort ();

int j;

struct S {
  static S* s[5];

  S () { s[j++] = this; }
  S (const S&) { s[j++] = this; }
  ~S () { 
    for (int k = 0; k < j; ++k)
      if (s[k] == this)
	return;
    abort ();
  }
};

S* S::s[5];

struct T {
  int i;
  S s;
};

T t;

T f () {
  return t;
}

void g (S) {
};

int main ()
{
  g (f ().s);
}

