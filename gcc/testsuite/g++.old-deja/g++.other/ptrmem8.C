// { dg-do assemble  }
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 22 Nov 2000 <nathan@codesourcery.com>

// bug 827. We died issuing warnings about dangerous pointer to member
// conversions.

struct bar
{
  int barm;
    static void a();
};
struct filler1 {int fm;};
struct filler2 {int fm;};
struct filler3 {int fm;};
struct filler4 {int fm;};

struct baz : filler1, bar, filler2
{
  int bazm;
};

struct foo : filler3, virtual baz, filler4
{
    static void a();
    void b() {}
    int m;
};

typedef void (bar::*barfPtr)();
typedef void (foo::*foofPtr)();
typedef int bar::*barmPtr;
typedef int foo::*foomPtr;

int main ()
{
  foofPtr fp = &foo::b;
  barfPtr bp = (barfPtr)fp;         // { dg-warning "" } pointer to member
  foofPtr fp2 = (foofPtr)bp;        // { dg-warning "" } pointer to member
  
  if (fp2 != fp)
    return 1;
  
  foo fobj;
  fobj.filler1::fm = 1;
  fobj.filler2::fm = 2;
  fobj.filler3::fm = 3;
  fobj.filler4::fm = 4;
  fobj.bazm = 5;
  fobj.barm = 6;
  fobj.m = 78;
  
  foomPtr fmp = &foo::m;
  barmPtr bmp = (barmPtr)fmp;          // { dg-warning "" } pointer to member
  foomPtr fmp2 = (foomPtr)bmp;         // { dg-warning "" } pointer to member
  bar *bptr = &fobj;
  
  if (fmp != fmp2)
    return 2;
  
  if (bptr->*bmp != 78)
    return 3;
  
  bp = reinterpret_cast <barfPtr> (fp);
  fp2 = reinterpret_cast <foofPtr> (bp);
  if (fp2 != fp)
    return 4;
  
  bmp = reinterpret_cast <barmPtr> (fmp);
  fmp2 = reinterpret_cast <foomPtr> (bmp);
  if (fmp != fmp2)
    return 5;
  
  return 0;
}
