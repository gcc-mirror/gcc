// PR target/77822
// { dg-do compile }

using UINT8 = char;
using UINT32 = int;
using UINT64 = long;
class A
{
  void m_fn1 ();
  struct B
  {
    UINT32 m_multiplier;
  };
  UINT8 m_datawidth;
  UINT8 m_subunits;
  B m_subunit_infos[];
};
int a;
UINT64 b;
void
A::m_fn1 ()
{
  int c = 32, d = m_datawidth / c;
  for (int e = 0; e < d; e++)
    {
      UINT32 f = e * 32;
      if (b >> f & 1)
	m_subunit_infos[m_subunits].m_multiplier = a;
    }
}
