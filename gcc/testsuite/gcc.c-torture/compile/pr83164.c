/* PR middle-end/83164 */

__PTRDIFF_TYPE__
foo (void)
{
  return (char *) foo - (char *) 0x1230;
}
