/* PR target/37483 */

unsigned long long
foo (unsigned count, int i)
{
  unsigned long long value;
  if (i == 0)
    value = (value & 0xFFFFFFFF) >> count;
  return value;
}
