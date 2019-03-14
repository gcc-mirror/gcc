/* PR tree-optimization/89703 */

typedef __SIZE_TYPE__ size_t;
extern char *strlen (const char *);
extern char *strnlen (const char *, size_t);
extern char c[2];

void
foo (char **q)
{
  q[0] = strlen (c);
  q[1] = strnlen (c, 2);
}
