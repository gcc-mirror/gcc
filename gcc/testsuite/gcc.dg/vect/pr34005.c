/* PR tree-optimization/34005 */
/* { dg-do compile } */

/* Testcase by Martin Michlmayr <tbm@cyrius.com> */

void XdmcpUnwrap (unsigned char *output, int k)
{
  int i;
  unsigned char blocks[2][8];
  k = (k == 0) ? 1 : 0;
  for (i = 0; i < 32; i++)
    output[i] = blocks[k][i];
}

/* { dg-final { cleanup-tree-dump "vect" } } */
