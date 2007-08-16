/* Testcase by Martin Michlmayr <tbm@cyrius.com> */

/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */

stream_test (void)
{
  static float input[20];
  int k;
  for (k = 0; k < 20; k++)
    input[k] = k * 1.0;
}

/* { dg-final { cleanup-tree-dump "vect" } } */
