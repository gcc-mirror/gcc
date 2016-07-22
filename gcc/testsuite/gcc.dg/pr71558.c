/* PR tree-optimization/71588 */

/* strcpy must not be pure, but make sure we don't ICE even when
   it is declared incorrectly.  */
char *strcpy (char *, const char *) __attribute__ ((__pure__));
__SIZE_TYPE__ strlen (const char *);
void *malloc (__SIZE_TYPE__);

char a[20];

char *
foo (void)
{
  __SIZE_TYPE__ b = strlen (a);
  char *c = malloc (b);
  return strcpy (c, a);
}
