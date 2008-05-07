/* PR middle-end/36137 */
extern void abort (void);

#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define MAX(a, b) ((a) > (b) ? (a) : (b))

int
main ()
{
  unsigned int u;
  int i = -1;

  u = MAX ((unsigned int) MAX (i, 0), 1);
  if (u != 1)
    abort ();

  u = MIN ((unsigned int) MAX (i, 0), (unsigned int) i);
  if (u != 0)
    abort ();
  return 0;
}
