// { dg-do run }
// { dg-options "-std=c++1z -Wno-deprecated" }
// { dg-require-weak "" }
// { dg-additional-sources "inline-var1a.C" }

#include "inline-var1.h"

static inline int var19 = bar (0);
static int inline var20 = bar (1);
extern inline int var23;
inline int var21 = foo (6);
inline int var22 = foo (7);
extern inline int var23, var22;
inline int var23 = foo (8);

static int v, w;

int
foo (int x)
{
  if (x != v++)
    __builtin_abort ();
  return 36 + x;
}

int
bar (int x)
{
  if (v < 6)
    __builtin_abort ();
  if ((x >> 4) != (w >> 4))
    {
      if ((x & 15) != 0 || (w & 15) != 2)
	__builtin_abort ();
      w = x + 1;
    }
  else if (x != w++)
    __builtin_abort ();
  return 46 + x;
}

int &ref1 = var1;
int &ref2 = N::var2;
const int &ref3 = S::var3;
int &ref4 = S::var4;
const int &ref5 = S::var5;
const int &ref6 = N::var6;
int &ref7 = var7;
double &ref8 = N::var8;
double &ref9 = S::var9;
const int &ref11 = S::var11;
int &ref12 = var12;
int &ref13 = var13;
int &ref14 = U::var14;
T &ref15 = U::var15;
T &ref16 = U::var16;
int &ref17 = U::var17;
const double &ref18 = U::var18;
int &ref19 = var19;
int &ref20 = var20;
int &ref21 = var21;
int &ref22 = var22;
int &ref23 = var23;
const int &ref24 = Y<int>::var24;
int &ref25 = Y<int>::var25;
int &ref26 = Y<int>::var26;
int &ref27 = var27<int>;
const int &ref28 = Y<int>::var28;
const char &ref24a = Y<char>::var24;
char &ref25a = Y<char>::var25;
int &ref26a = Y<char>::var26;
char &ref27a = var27<char>;
const char &ref28a = Y<char>::var28;
extern int &alt1;
extern int &alt2;
extern const int &alt3;
extern int &alt4;
extern const int &alt5;
extern const int &alt6;
extern int &alt7;
extern double &alt8;
extern double &alt9;
extern const int &alt11;
extern int &alt12;
extern int &alt13;
extern int &alt14;
extern T &alt15;
extern T &alt16;
extern int &alt17;
extern const double &alt18;
extern int &alt19;
extern int &alt20;
extern int &alt21;
extern int &alt22;
extern int &alt23;
extern const int &alt24;
extern int &alt25;
extern int &alt26;
extern int &alt27;
extern const int &alt28;
extern const char &alt24a;
extern char &alt25a;
extern int &alt26a;
extern char &alt27a;
extern const char &alt28a;

int
main ()
{
  if (v != 9)
    __builtin_abort ();
  if (var1 != 4
      || N::var2 != 0
      || S::var3 != 5
      || S::var4 != 6
      || S::var5 != 7
      || N::var6 != 8
      || var7 != 9
      || N::var8 != 2.0
      || S::var9 != 3.0
      || sizeof (N::var10) != 1
      || S::var11 != 11
      || var12 != 36
      || var13 != 37
      || U::var14 != 38
      || U::var15.t != 39
      || U::var16.t != 40
      || U::var17 != 41
      || U::var18 != 4.0
      || var19 != 46
      || var20 != 47
      || var21 != 42
      || var22 != 43
      || var23 != 44
      || Y<int>::var24 != 6
      || Y<int>::var25 != 7
      || Y<int>::var26 != 8
      || var27<int> != 9
      || Y<int>::var28 != 10
      || Y<char>::var24 != 6
      || Y<char>::var25 != 7
      || Y<char>::var26 != 8
      || var27<char> != 9
      || Y<char>::var28 != 10)
    __builtin_abort ();
  if (ref1 != 4
      || ref2 != 0
      || ref3 != 5
      || ref4 != 6
      || ref5 != 7
      || ref6 != 8
      || ref7 != 9
      || alt7 != 9
      || ref8 != 2.0
      || alt8 != 2.0
      || ref9 != 3.0
      || ref11 != 11
      || ref12 != 36
      || ref13 != 37
      || ref14 != 38
      || ref15.t != 39
      || ref16.t != 40
      || ref17 != 41
      || ref18 != 4.0
      || ref19 != 46
      || alt19 != 62
      || ref20 != 47
      || alt20 != 63
      || ref21 != 42
      || ref22 != 43
      || ref23 != 44
      || ref24 != 6
      || ref25 != 7
      || ref26 != 8
      || ref27 != 9
      || ref28 != 10
      || ref24a != 6
      || ref25a != 7
      || ref26a != 8
      || ref27a != 9
      || ref28a != 10)
    __builtin_abort ();
  if (&ref1 != &alt1
      || &ref2 != &alt2
      || &ref3 != &alt3
      || &ref4 != &alt4
      || &ref5 != &alt5
      || &ref6 != &alt6
      || &ref7 == &alt7
      || &ref8 == &alt8
      || &ref9 != &alt9
      || &ref11 != &alt11
      || &ref12 != &alt12
      || &ref13 != &alt13
      || &ref14 != &alt14
      || &ref15 != &alt15
      || &ref16 != &alt16
      || &ref17 != &alt17
      || &ref18 != &alt18
      || &ref19 == &alt19
      || &ref20 == &alt20
      || &ref21 != &alt21
      || &ref22 != &alt22
      || &ref23 != &alt23
      || &ref24 != &alt24
      || &ref25 != &alt25
      || &ref26 != &alt26
      || &ref27 != &alt27
      || &ref28 != &alt28
      || &ref24a != &alt24a
      || &ref25a != &alt25a
      || &ref26a != &alt26a
      || &ref27a != &alt27a
      || &ref28a != &alt28a)
    __builtin_abort ();
}
