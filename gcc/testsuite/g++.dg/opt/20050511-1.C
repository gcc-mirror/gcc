/* { dg-do run } */
/* { dg-options "-O3" { target powerpc*-*-* } } */
#include <stdio.h>
#include <stdlib.h>

typedef signed short SINT16 ;
typedef unsigned long UINT32 ;
typedef unsigned int UINT ;

class A
{
public:
    union   
    {
      SINT16 xy[2];
      UINT32 abXY;
    };
  bool operator==(const A& other) const {return abXY == other.abXY;}
  bool operator!=(const A& other) const {return abXY != other.abXY;}
};

template <int size> struct pArray { unsigned char u08[16*(((size*1)+15)/16)] __attribute__ ((aligned(16))); };

struct B
{
  union {
    A mvL[2];
    pArray<1> xyz;
  };
} ;

typedef struct
{
  UINT w;
  B b;

}C;


UINT32 bar (const C * sPtr)
{
  UINT w = sPtr->w;
  A a;

  a.xy[0] = sPtr->b.mvL[w].xy[0]<<2;
  a.xy[1] = sPtr->b.mvL[w].xy[1]<<2;

  if (a.xy[0] != ((SINT16) 0xffff << 2))
	abort ();
}

int main()
{
	A a;
	C c;
	a.xy[0] = 0xffff;
	a.xy[1] = 0xffff;
	c.w=0;
	c.b.mvL[0].xy[0] = a.xy[0];
	c.b.mvL[0].xy[1] = a.xy[1];

	bar (&c);
}
	
