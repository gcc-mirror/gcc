static inline void
foo (char accept)
{
  char s;
  while (s == accept) ;
}

static void
bar (void)
{
  char ch;
  foo (ch);
}
