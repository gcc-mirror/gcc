void abort (void);
void exit (int);

typedef struct
{
  char x;
} T;

T
f (s1)
     T s1;
{
  T s1a;
  s1a.x = s1.x;
  return s1a;
}

int
main (void)
{
  T s1a, s1b;
  s1a.x = 100;
  s1b = f (s1a);
  if (s1b.x != 100)
    abort ();
  exit (0);
}
