char headline[256];
struct hdr {
  char part1[9];
  char part2[8];
} p;

void __attribute__((noinline,noclone))
init()
{
  __builtin_memcpy (p.part1, "FOOBARFOO", sizeof (p.part1));
  __builtin_memcpy (p.part2, "SPEC CPU", sizeof (p.part2));
}

int main()
{
  char *x;
  int c;
  init();
  __builtin_memcpy (&headline[0], p.part1, 9);
  c = 9;
  x = &headline[0];
  x = x + c;
  __builtin_memset (x, ' ', 245);
  __builtin_memcpy (&headline[10], p.part2, 8);
  c = 18;
  x = &headline[0];
  x = x + c;
  __builtin_memset (x, ' ', 238);
  if (headline[10] != 'S')
    __builtin_abort ();
  return 0;
}
