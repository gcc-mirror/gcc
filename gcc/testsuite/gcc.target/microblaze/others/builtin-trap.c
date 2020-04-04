/* { dg-do compile } */

void trap ()
{
  __builtin_trap ();
}

/* { dg-final { scan-assembler "bri\t0" } } */
