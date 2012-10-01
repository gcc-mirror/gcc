int *b;
void *d;
int c;
static int *f1 ();
void f2 ()
{
  int *a = f1 (0);
}

int *f1 (j)
{
  b = malloc (0);
  d = *malloc;
  c = j;
}

