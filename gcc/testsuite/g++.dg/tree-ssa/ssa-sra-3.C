/* { dg-do compile } */
/* { dg-options "-O2" } */
/* Test check use_block_copy bit propagation in sra element hierarchy.  */

typedef unsigned char UINT8 ;
typedef unsigned int UINT ;
class C4
{
public:
  int xy[2];
};

class C3
{
public:
  inline void
  Reset()
  {
    C4 const mvMax = {0x7fff, 0x7fff};

    m42(0,mvMax); 
    m42(1,mvMax);
    m43(0);
  };

  inline void m42 (UINT  i, C4 mv)
  {
    mMv[i] = mv;
  };



  inline void  m43(UINT j)
  {
    m44 (j);
    d41 = j + 1;
  };

private:

  C4 mMv[2];
  UINT8 d41;
  inline void m44 (UINT j) const {};
};

class C2
{
private:
  bool valid;
};

class C1
{
public:
  void m1(C3 *c);

private:
  const C2 * d1[2];
  void m2(C3 *m);
};                                                                                                                                                                           

void C1::m1 (C3 *r)
{
  C3 x;
  m2(&x);
}
void C1::m2(C3 *x)
{
  C3 m3;
  int i;
  m3.Reset ();
  for(i=0; i<2; i++)
    {
      const C2 * r = d1[i];
      if (r!=__null)
        {
	  C4 const c400 = {0,0};
          m3.m42 (i, c400);
	  
        }
    }
}   



