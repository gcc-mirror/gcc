// PR tree-optimization/56381
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -w" }

template <class>
class intrusive_ptr {};
class BasicReferenceCounted
{
};
template <class T>
class ReferenceCountingPointer : intrusive_ptr <T>
{
};
typedef BasicReferenceCounted ReferenceCountedInConditions;
class PointTag;
template <typename T, typename>
struct PreciseFloatType
{
  typedef T Type;
};
template <typename T, int N>
struct ExtVecTraits
{
  typedef T __attribute__ ((vector_size (N * sizeof (T)))) type;
};
template <typename T, int N>
using ExtVec = typename ExtVecTraits <T, N>::type;
template <typename T> using Vec4 = ExtVec <T, 4>;
template <typename Vec>
Vec cross3 (Vec x, Vec y)
{
  Vec x1200 = (Vec) { x[2], x[0] };
  Vec y2010 { y[2], y[0], y[1], y[0] };
  Vec x2010 = (Vec) { x[2], x[0], x[1], x[0] };
  Vec y1200 = (Vec) { y[1], y[0] };
  return x1200 * y2010 - x2010 * y1200;
}
template <typename T>
struct Rot3
{
  typedef Vec4 <T> Vec;
  Vec axis[3];
};
class Basic2DVector
{
};
template <typename T>
struct Basic3DVector
{
  typedef Vec4 <T> MathVector;
  Basic3DVector (MathVector iv) : v { (iv[0]), (iv[1]), (iv[2]), (iv[3]) } {}
  T mag2 () {}
  Basic3DVector unit ()
  {
    T my_mag = mag2 ();
    return (my_mag) ? (*this) * (T () / (my_mag)) : *this;
  }
  Basic3DVector
  cross (Basic3DVector lh) { return cross3 (v, lh.v); }
  Vec4 <T> v;
};
template <class T>
Basic3DVector <T> operator * (Basic3DVector <T>, T);
template <class T, class, class>
struct PV3DBase
{
  typedef Basic3DVector <T> BasicVectorType;
  template <class U>
  PV3DBase (Basic3DVector <U> v) : theVector (v) {}
  BasicVectorType basicVector () { return theVector; }
  T x ();
  T y ();
  BasicVectorType theVector;
};
class VectorTag;
template <class T, class FrameTag>
struct Vector3DBase:public PV3DBase <T, VectorTag, FrameTag>
{
  typedef PV3DBase <T, VectorTag, FrameTag> BaseClass;
  template <class U>
  Vector3DBase (Basic3DVector <U> v) : BaseClass (v) {}
  Vector3DBase unit () { return (this->basicVector ().unit ()); }
  template <class U>
  Vector3DBase <typename PreciseFloatType <T, U>::Type, FrameTag> cross (Vector3DBase <U, FrameTag> v)
  {
    return (this->theVector.cross (v.basicVector ()));
  }
};
template <class T, class FrameTag>
class Point3DBase : public PV3DBase <T, PointTag, FrameTag>
{
};
template <typename T, typename U, class Frame>
Vector3DBase <typename PreciseFloatType <T, U>::Type, Frame> operator - (Point3DBase <T, Frame>, Point3DBase <U, Frame>);
class GlobalTag;
template <class T>
struct TkRotation
{
  typedef Vector3DBase <T, GlobalTag> GlobalVector;
  TkRotation (GlobalVector aX, GlobalVector aY)
  {
    GlobalVector uX = aX.unit ();
    GlobalVector uY = aY.unit ();
    GlobalVector uZ (uX.cross (uY));
    rot.axis[2] = uZ.basicVector ().v;
  }
  Basic3DVector <T> z ();
  Rot3 <T> rot;
};
template <class T>
struct GloballyPositioned
{
  typedef Point3DBase <T, GlobalTag> PositionType;
  typedef TkRotation <T> RotationType;
  typedef Point3DBase <T, GlobalTag> GlobalPoint;
  typedef Vector3DBase <T, GlobalTag> GlobalVector;
  T iniPhi () { return 999.9978; }
  GloballyPositioned (PositionType pos, RotationType rot) : thePos (pos), theRot (rot) { resetCache (); }
  PositionType position () const;
  RotationType rotation () const;
  PositionType thePos;
  RotationType theRot;
  void resetCache ()
  {
    if ((thePos.x () == 0.) && (thePos.y () == 0.))
      thePhi = 0.;
    else
      thePhi = iniPhi ();
  }
  T thePhi;
};
class Plane;
using TangentPlane = Plane;
struct Surface : public GloballyPositioned <float>, ReferenceCountedInConditions
{
  typedef GloballyPositioned <float> Base;
  Surface (PositionType pos, RotationType rot):
  Base (pos, rot) {}
};
struct Plane : Surface
{
  template <typename ... Args>
  Plane (Args ... args):
  Surface ((args) ...) {}
};
class Cylinder : Surface
{
  void tangentPlane (const GlobalPoint &) const;
};
void
Cylinder::tangentPlane (const GlobalPoint & aPoint) const
{
  GlobalVector yPlane (rotation ().z ());
  GlobalVector xPlane (yPlane.cross (aPoint - position ()));
  new TangentPlane (aPoint, RotationType (xPlane, yPlane));
}
