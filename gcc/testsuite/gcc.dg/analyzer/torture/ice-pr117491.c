short c;
int x;
void *d, *s;

void
foo()
{
  __builtin_memset(&c, !x, sizeof c);
  __builtin_memcpy(d, s, c);
}
