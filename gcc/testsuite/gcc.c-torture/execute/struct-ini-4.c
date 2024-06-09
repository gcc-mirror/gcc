void abort (void);
void exit (int);

struct s {
  int a[3];
  int c[3];
};

struct s s = {
  c: {1, 2, 3}
};

int
main(void)
{
  if (s.c[0] != 1)
    abort ();
  exit (0);
}
