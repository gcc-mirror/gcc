/* { dg-do compile } */
/* { dg-options "-O0" } */

__attribute__ ((target_version ("default"))) int
foo ()
{
  return 1;
}

__attribute__ ((target_version ("rng"))) int
foo ()
{
  return 2;
}

__attribute__ ((target_version ("flagm"))) int
foo ()
{
  return 3;
}

__attribute__ ((target_version ("rng+flagm"))) int
foo ()
{
  return 4;
}

int
bar ()
{
  return foo ();
}

/* Check usage of the first two FMV features, in case of off-by-one errors.  */
/* { dg-final { scan-assembler-times "\nfoo\.default:\n" 1 } } */
/* { dg-final { scan-assembler-times "\nfoo\._Mrng:\n" 1 } } */
/* { dg-final { scan-assembler-times "\nfoo\._MrngMflagm:\n" 1 } } */
/* { dg-final { scan-assembler-times "\nfoo\._Mflagm:\n" 1 } } */

/* { dg-final { scan-assembler-times "\nfoo\.resolver:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\tbl\tfoo\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.type\tfoo, %gnu_indirect_function\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.set\tfoo,foo\.resolver\n" 1 } } */
