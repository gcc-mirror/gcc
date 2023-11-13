void abort (void);
void exit (int);

struct {
  int a:4;
  int :4;
  int b:4;
  int c:4;
} x = { 2,3,4 };

int
main (void)
{
  if (x.a != 2)
    abort ();
  if (x.b != 3)
    abort ();
  if (x.c != 4)
    abort ();
  exit (0);
}
