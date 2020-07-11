/* { dg-do compile } */

__attribute__ ((target("branch-protection=bti")))
int foo ()
{
label:
  return 0;
}

/* { dg-final { scan-assembler-not {hint (36|38) // bti (j|jc)} } } */
