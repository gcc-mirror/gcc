// Build don't link:

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
    void b() {};
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
    barfPtr bp = static_cast <barfPtr> (fp);    // ERROR - invalid static_cast
    foofPtr fp2 = static_cast <foofPtr> (bp);   // ERROR - invalid static_cast
    foofPtr fp3 = bp;                           // ERROR - cannot convert
    fp3 = (foofPtr)bp;                          // WARNING - via virtual base
    
    foomPtr fmp = &foo::m;
    barmPtr bmp = static_cast <barmPtr> (fmp);  // ERROR - invalid static_cast
    foomPtr fmp2 = static_cast <foomPtr> (bmp); // ERROR - invalid static_cast
    foomPtr fmp3 = bmp;                         // ERROR - cannot convert
    fmp3 = (foomPtr)bmp;                        // WARNING - via virtual base
  }
  
  return 0;
}
