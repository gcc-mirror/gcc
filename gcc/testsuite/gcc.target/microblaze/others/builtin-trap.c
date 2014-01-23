/* { dg-do compile } */

void trap ()
{
  __builtin_trap ();
}

/* { dg-final { scan-assembler "brki\tr0,-1" } } */