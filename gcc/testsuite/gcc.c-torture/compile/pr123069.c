/* PR middle-end/123069 */

__attribute__((__vector_size__ (2 * sizeof (long long)))) unsigned long long v;

void
foo (void)
{
  v *= 0xffffffff00000000ULL;
}
