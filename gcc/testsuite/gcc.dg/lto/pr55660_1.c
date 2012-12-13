extern int foo (char*);
extern void abort (void);

extern char n[3];

int main ()
{
  int i, m = 0;
  for (i = 0; i < 3; i++)
    m += foo(&n[i]);

  if (m != 'b')
    abort ();
  return 0;
}
