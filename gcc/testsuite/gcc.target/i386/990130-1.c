/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options -O0 } */

typedef 	 int SItype	__attribute__ ((mode (SI)));
typedef		 int DItype	__attribute__ ((mode (DI)));
typedef unsigned int USItype	__attribute__ ((mode (SI)));
  struct DIstruct {SItype low, high;};
typedef union
{
  struct DIstruct s;
  DItype ll;
} DIunion;
DItype
__muldi3 (DItype u, DItype v)
{
  DIunion w;
  DIunion uu, vv;
  uu.ll = u,
  vv.ll = v;
  w.ll = ({DIunion __w;	__asm__ ("mull %3"	: "=a" ((USItype) (  __w.s.low )),	"=d" ((USItype) ( __w.s.high ))	: "%0" ((USItype) (   uu.s.low  )),	"rm" ((USItype) (    vv.s.low  ))) ;	__w.ll; }) ;
  w.s.high += ((USItype) uu.s.low * (USItype) vv.s.high
	       + (USItype) uu.s.high * (USItype) vv.s.low);
  return w.ll;
}
