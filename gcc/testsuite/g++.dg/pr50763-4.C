/* { dg-do compile } */
/* { dg-options "-O2 -ftree-tail-merge" } */

float
clamp (const float x)
{
  return x <= 1 ? 1 : x;
}

template < class T > struct VECTOR
{
  float x;
};
template < class TV > class JOINT
{
  virtual void Constrain_Angles (VECTOR < float >&angles) const;
};

template < class TV > class ANGLE_JOINT:public JOINT < TV >
{
  virtual ~ ANGLE_JOINT ()
  {
  }
  void Constrain_Angles (VECTOR < float >&angles) const
  {
    VECTOR < float >v;
    if (v.x)
        v.x = clamp (angles.x);
    else
        v.x = angles.x;
      angles.x = v.x;
  }
};
template ANGLE_JOINT < int >::~ANGLE_JOINT ();
