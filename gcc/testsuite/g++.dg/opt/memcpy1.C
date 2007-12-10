// PR target/34403
// Origin: Martin Michlmayr <tbm@cyrius.com>

// { dg-do compile }
// { dg-options "-O" }

typedef unsigned char uint8_t;
typedef uint8_t uint8;
typedef long unsigned int size_t;
class csVector2
{
public:float x;
};
class csBox2
{
};
struct iBase
{
};
struct iClipper2D:public virtual iBase
{
};
template < class Class > class scfImplementation:public virtual iBase
{
};
template < class Class, class I1 > class scfImplementation1:public
scfImplementation < Class >,
  public I1
{
};
class csClipper:public scfImplementation1 < csClipper, iClipper2D >
{
};
class csBoxClipper:public csClipper
{
  csBox2 region;
  virtual uint8 Clip (csVector2 * InPolygon, size_t InCount,
                      csVector2 * OutPolygon, size_t & OutCount);
};
struct StatusOutputNone
{
};
namespace CS
{
  template < typename BoxTest, typename StatusOutput > class BoxClipper
  {
    BoxTest boxTest;
    StatusOutput statOut;
    const csBox2 & region;
    csVector2 *InP;
    size_t InV;
    csVector2 *OutP;
    size_t OutV;
  public:  BoxClipper (const BoxTest & boxTest, const StatusOutput & statOut,
                  const csBox2 & region, csVector2 * InP, size_t InV,
                  csVector2 * OutP):boxTest (boxTest), statOut (statOut),
      region (region), InP (InP), InV (InV), OutP (OutP), OutV (-1)
    {
    }
    uint8 Clip ()
    {
      __builtin_memcpy (this->OutP, InP, OutV * sizeof (csVector2));
    }
  };
}
struct BoxTestAll
{
};
uint8
csBoxClipper::Clip (csVector2 * InPolygon, size_t InCount,
                    csVector2 * OutPolygon, size_t & OutCount)
{
  BoxTestAll b;
  StatusOutputNone n;
  CS::BoxClipper < BoxTestAll, StatusOutputNone > boxClip (b, n, region,
                                                           InPolygon, InCount,
                                                           OutPolygon);
  uint8 Clipped = boxClip.Clip ();
}
