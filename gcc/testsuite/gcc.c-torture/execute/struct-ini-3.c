void abort (void);
void exit (int);

struct
{
  unsigned int f1:1, f2:1, f3:3, f4:3, f5:2, f6:1, f7:1;
} result = {1, 1, 7, 7, 3, 1, 1};

int
main (void)
{
  if ((result.f3 & ~7) != 0 || (result.f4 & ~7) != 0)
    abort ();
  exit (0);
}
