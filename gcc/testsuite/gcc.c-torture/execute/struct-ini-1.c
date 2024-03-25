void abort (void);
void exit (int);

struct S
{
  char f1;
  int f2[2];
};

struct S object = {'X', 8, 9};

int
main (void)
{
  if (object.f1 != 'X' || object.f2[0] != 8 || object.f2[1] != 9)
    abort ();
  exit (0);
}

