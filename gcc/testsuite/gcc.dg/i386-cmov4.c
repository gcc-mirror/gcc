/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -march=k8" } */
/* { dg-final { scan-assembler "cmov" } } */

/* Verify that if conversion happends for memory references.  */
int ARCHnodes;
int *nodekind;
float *nodekindf;
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
