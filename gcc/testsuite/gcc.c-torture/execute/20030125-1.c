/* PR optimization/9279 */
/* Verify that the combiner zero-extends
   variables correctly.   */

extern void abort (void);

int x = 0;
char c, a = -1;
char *p = &c;

int main (void)
{
  if ((x ? 0 : (unsigned char)(*p = a)) == -1)
    abort();

  return 0;
}
