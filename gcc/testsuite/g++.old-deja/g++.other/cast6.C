// { dg-do assemble  }

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 28 Nov 2000 <nathan@codesourcery.com>

// We failed to reject static_cast and implicit conversions of pointers to
// member that traversed a virtual base.

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

struct X;
typedef void (X::*xfPtr) ();
typedef int X::*xmPtr;

int main ()
{
  {
    foofPtr fp = &foo::b;
    barfPtr bp = static_cast <barfPtr> (fp);    // { dg-error "via virtual base" } invalid static_cast
    foofPtr fp2 = static_cast <foofPtr> (bp);   // { dg-error "via virtual base" } invalid static_cast
    foofPtr fp3 = bp;                           // { dg-error "via virtual base" } cannot convert
    fp3 = (foofPtr)bp;                          // { dg-error "via virtual base" } via virtual base
    
    foomPtr fmp = &foo::m;
    barmPtr bmp = static_cast <barmPtr> (fmp);  // { dg-error "via virtual base" } invalid static_cast
    foomPtr fmp2 = static_cast <foomPtr> (bmp); // { dg-error "via virtual base" } invalid static_cast
    foomPtr fmp3 = bmp;                         // { dg-error "via virtual base" } cannot convert
    fmp3 = (foomPtr)bmp;                        // { dg-error "via virtual base" } via virtual base
  }
  
  return 0;
}
