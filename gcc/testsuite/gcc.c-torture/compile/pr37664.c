/* PR tree-optimization/37664 */

int v;

int
foo ()
{
  int a = 0x8899A862;
  int b = 0x8E * a;
  int c = (b % b);
  if (v > (4294967295U >> c))
    return v;
  return 0;
}
