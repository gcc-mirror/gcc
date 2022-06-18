/* PR middle-end/105998 */

typedef int __attribute__((__vector_size__ (sizeof (long long)))) V;

V v;

long long
foo (void)
{
  long long l = (long long) ((0 | v) - ((V) { } == 0));
  return l;
}
