/* { dg-do compile } */
/* { dg-options "-O2 -march=k8" } */
/* { dg-final { scan-assembler "cmov\[^4\]" } } */

/* Verify that if conversion happends for memory references.  */
int ARCHnodes;
int *nodekind;
float *nodekindf;
void
t()
{
int i;
/* Redefine nodekind to be 1 for all surface nodes */

  for (i = 0; i < ARCHnodes; i++) {
    nodekind[i] = (int) nodekindf[i];
    if (nodekind[i] == 3)
      nodekind[i] = 1;
  }
}
