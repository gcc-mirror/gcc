// GROUPS passed copy-ctors
extern "C" void printf (char *, ...);
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
    printf ("FAIL\n");
  else
    printf ("PASS\n");
}
