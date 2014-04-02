// { dg-do compile }

struct Layer;
template <typename> struct A
{
  Layer *m_fn1 ();
  Layer &operator[](int p1) { return m_fn1 ()[p1]; }
};
struct Layer
{
};
void fn1 (A<int> &p1, int Layer::*p2, int p3)
{
  for (int a = 0;; ++a, ++p3)
    p1[p3].*p2 = p1[a].*p2;
}
