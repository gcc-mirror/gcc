/* PR tree-optimization/34371 */
/* { dg-require-stack-size "1108" } */
/* Testcase by Martin Michlmayr <tbm@cyrius.com> */

void centerln (int width, int ch, char *s)
{
  unsigned int sidebar;
  int i;
  char linet1[1000];
  char linet2[100];
  sidebar = (width - __builtin_strlen (s)) / 2;
  for (i = 0; i < sidebar; i++)
    linet2[i] = ch;
  __builtin_strcpy (linet1, linet2);
}
