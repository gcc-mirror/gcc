typedef void (*F) (void);
void bar (F);

void
foo (void *a, int b)
{
  bar ((F) a + b);
}
