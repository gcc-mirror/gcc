// GROUPS passed groff
/* This should compile properly with the new overloading scheme.  */

extern "C" int printf (const char *, ...);
extern "C" void exit (int);

int win = 0;

class symbol
{
public:
  symbol(const char *p, int how = 0) {}
  symbol() {}
};

class dictionary
{
public:
  void *lookup(symbol s, void *v=0) { win = 1; }
  void *lookup(const char *) {}
};

int main()
{
  char buf[2048];
  dictionary exceptions;
  unsigned char *tem = new unsigned char[19 + 1];

  exceptions.lookup (symbol (buf), tem);

  printf (win ? "PASS\n" : "FAIL\n");
  exit (! win);
}
