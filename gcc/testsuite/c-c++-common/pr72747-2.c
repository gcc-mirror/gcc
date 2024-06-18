/* { dg-do compile } */
/* { dg-options "-c -maltivec -fdump-tree-gimple" } */
/* { dg-require-effective-target powerpc_altivec } */

/* PR 72747: test that cascaded definition is happening for non constants. */

void foo ()
{
  extern int i;
  __vector int v,w;
    v = w = (vector int) { i };
}

int main (int argc, char *argv[])
{
  return 0;
}
/* { dg-final { scan-tree-dump-times " w = {i.0_1}" 1 "gimple" } } */
