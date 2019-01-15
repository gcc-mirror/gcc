/* { dg-do "compile" } */

void __attribute__ ((target("branch-protection=pac-ret+leaf,branch-protection=none")))
foo ()
{
}

void __attribute__ ((target("branch-protection=pac-ret,branch-protection=none")))
foo2 ()
{
  /* Function call here to make this a non-leaf function, so that it is covered by pac-ret.  */
  foo ();
}

/* { dg-final { scan-assembler-not "\tautiasp\t" } } */
/* { dg-final { scan-assembler-not "\tpaciasp\t" } } */
