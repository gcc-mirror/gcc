// { dg-do run  }
// GROUPS passed copy-ctors
extern "C" int printf (const char *, ...);
int count = 0;

class C {
public:
  C (int) { count++; }
  operator int () { return 0; }
};

int
main ()
{
  C c1 (1);
  C c2 (c1);

  if (count != 1)
    { printf ("FAIL\n"); return 1; }
  else
    printf ("PASS\n");
}
