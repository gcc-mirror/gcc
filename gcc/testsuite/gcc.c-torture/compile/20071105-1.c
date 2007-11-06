/* PR tree-optimization/33993 */
/* Testcase by Martin Michlmayr <tbm@cyrius.com> */

void
init_full (char *array, int ny)
{
  int j;
  char acc = 128;
  for (j = 0; j < ny; j++)
    *array++ = acc++;
}
