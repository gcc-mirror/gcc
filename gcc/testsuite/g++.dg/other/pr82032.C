/* { dg-do compile } */
/* { dg-options "-O3 -Wno-return-type" } */

template <typename a> class b
{
public:
  typename a::aa operator[] (typename a::c) { }
};
class d
{
public:
  typedef long c;
  typedef int aa;
};
struct e
{
  int af[4];
  int ag;
};
b<d> f;
bool
g (e &i)
{
  for (int h; h; ++h)
    switch (f[h])
      {
      case 'x':
      case 'a':
	i.af[h] = 3;
	break;
      default:
	return false;
      }

  return true;
}
