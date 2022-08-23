/* { dg-do compile } */

void sink(const char*);
static const char *a;
int main()
{
  const char *b = a;
  for (int i = 0; i < 2; ++i)
    while (*b++)
      ;
  sink(b);
  return 0;
}
