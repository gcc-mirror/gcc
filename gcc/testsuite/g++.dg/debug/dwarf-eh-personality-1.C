// { dg-options "-fno-dwarf2-cfi-asm" }

extern void bar (void);
int foo (void)
{
  try {
      bar();
  } catch (...) {
      return 1;
  }
  return 0;
}

void foobar (void)
{
}

