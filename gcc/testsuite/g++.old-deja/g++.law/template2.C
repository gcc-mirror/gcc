// GROUPS passed templates
// Special g++ Options: -fguiding-decls
extern "C" int printf (const char *, ...);

template<class T> T max(T a, T b) { return a > b ? a : b; }

int max(int, int);

main()
{
  int j;

  j = max(1,2);
  j = max (1, 'c');
  printf ("PASS\n");
}

