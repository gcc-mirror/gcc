// { dg-do run  }
// Testcase for precedence of ?: wrt =

extern "C" int printf (const char *, ...);

int main()
{
  int j = 0, k = 0;
  1 ? j : k = 5;		// should be parsed 1 ? j : (k = 5)
  (void) (1 ? k = 5 : 0);
  k = 5 ? 1 : 0;		// should be parsed k = (5 ? 1 : 0)

  printf ("%d %d\n", j, k);

  return j == 5 || k == 5;
}
