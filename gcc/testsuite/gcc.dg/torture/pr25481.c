/* { dg-do compile } */

struct s {
  int *blah;
};

static struct s array[] = {
  { 0 }
};

void
foo (struct s *p)
{
  unsigned int n = 1;
  struct s *q = &array[n];
  while (p < q)
    p++;
}
