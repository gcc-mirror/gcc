// { dg-do run  }
// Positive testcase for decls in conditions.

extern "C" int printf(const char *, ...);

int up = 0;
int down = 0;

struct T
{
  int i;
  T(int j) { i = j; printf("UP\n"); up++; }
  T(const T& t) { i = t.i; printf("unwanted copy\n"); }
  ~T() { printf ("DOWN\n"); down++; }
  operator int () { return i; }
};

int main ()
{
  int t;

  if (T t = 1)
    ;

  printf ("\n");
  
  int j = 3;
  while (T t = j--)
    ;
  
  printf ("\n");
  
  j = 3;
  while (1)
    {
      T t = j--;
      if (t) continue;
      break;
    }
  
  printf ("\n");
  
  j = 3;
  for (;T t = j--;)
    ;

  printf ("\n");
  
  for (int k = 3; T t = k--;)
    ;

  printf ("\n");
  
  switch (T t = 34)
    {
    case 34:
      ;
    }

  printf ("\n");
  
  if (up == down && up == 18)
    return 0;
  else
    return 1;
}
