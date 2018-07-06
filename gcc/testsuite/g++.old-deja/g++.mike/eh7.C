// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

int
main() {
  if (0)
    throw 1 | 2;
}
