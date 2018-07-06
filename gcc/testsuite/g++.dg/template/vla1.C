// PR c++/29226
// { dg-options "" }

template <bool>
static int label (int w)
{
  sizeof(int[w]);
  return 0;
}
int a = label<false>(1);
