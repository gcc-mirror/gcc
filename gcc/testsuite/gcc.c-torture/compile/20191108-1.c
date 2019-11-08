/* PR target/92095 */
/* Testcase by Sergei Trofimovich <slyfox@inbox.ru> */

typedef union {
  double a;
  int b[2];
} c;

double d(int e)
{
  c f;
  (&f)->b[0] = 15728640;
  return e ? -(&f)->a : (&f)->a;
}
