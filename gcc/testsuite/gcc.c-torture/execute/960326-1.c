void abort (void);
void exit (int);

struct s
{
  int a;
  int b;
  short c;
  int d[3];
};

struct s s = { .b = 3, .d = {2,0,0} };

int
main (void)
{
  if (s.b != 3)
    abort ();
  exit (0);
}
