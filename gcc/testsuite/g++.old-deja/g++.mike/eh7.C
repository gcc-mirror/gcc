// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

main() {
  if (0)
    throw 1 | 2;
}
