/* For a short time on the tree-ssa branch this would warn that
   value was not initialized as it was optimizing !(value = (m?1:2))
   to 0 and not setting value before.  */

int t(int m)
{
  int value;
  if (!(value = (m?1:2)))
    value = 0;
  return value;
}
