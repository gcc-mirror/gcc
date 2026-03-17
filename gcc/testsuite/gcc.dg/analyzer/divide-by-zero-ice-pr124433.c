unsigned m;
int c;

void
foo()
{
  do c %= (5ull << 40) & m;
  while (c);
}
