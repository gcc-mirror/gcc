/* PR ipa/98075 */
/* { dg-do compile } */                                                                        
/* { dg-options "-O2 -fno-inline" } */

template <typename BS>
class xg {
public:
  BS *
  fw ()
  {
    return static_cast<BS *> (operator new (sizeof (BS)));
  }
};

class zp : xg<int> {
public:
  __attribute__ ((always_inline)) zp ()
  {
    hy = xg<int>::fw ();
  }

private:
  int *hy;
};

void
e5 ()
{
  zp ix;
}
