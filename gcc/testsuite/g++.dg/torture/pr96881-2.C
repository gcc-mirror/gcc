/* { dg-do run } */

struct S { int s; ~S () {} } s;

void __attribute__((noipa))
foo (int flag)
{
  s.s = 1;
  // We have to makes sure to not make the inlined CLOBBER
  // unconditional but we have to remove it to be able
  // to elide the branch
  if (!flag)
    return;
  s.~S();
}

void __attribute__((noipa))
bar (int flag)
{
  s.s = 1;
  // CD-DCE chooses an arbitrary path, try to present it
  // with all variants
  if (flag)
    s.~S();
}

int
main ()
{
  foo (0);
  if (s.s != 1)
    __builtin_abort ();
  bar (0);
  if (s.s != 1)
    __builtin_abort ();
  return 0;
}
