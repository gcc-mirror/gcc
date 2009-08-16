/* { dg-options "-g3 -O" } */
/* { dg-do compile } */

/* Here's the deal: f4 is inlined into main, f3 is inlined into f4, f2 is
   inlined into f1. The DIE of main should contain DW_TAG_inlined_subroutines
   children for f4, f3, f2 and f1. Also, there should be a DIE representing
   and out of line instance of f4, aside the DIE representing its abstract
   instance.
   We used to fail to emit debugging info for t1, because it was moved
   inside the (inlined) block of f1, marked as abstract, then we'd crash.  */

#define UNUSED __attribute__((unused))
#define EXT __extension__

int undef(void);

inline static void
f1 (int i UNUSED)
{
}

inline static void
f2 (void)
{
  f1 (EXT ({ int t1 UNUSED; undef (); }));
}

inline static void
f3 (void)
{
  int v1 UNUSED;
  int v2 UNUSED;

  EXT ({ int t2 UNUSED; if (0) undef (); 0; })
    && EXT ({ int t3 UNUSED; if (0) undef (); 0; });

  if (1)
    {
      undef ();
      if (1)
        f2 ();
    }

  {
    undef ();
  }
}

inline static void
f4 (void)
{
  EXT ({ undef (); 1; }) && EXT ({ int t4 UNUSED = ({ 1; }); 1; });

  { }

  EXT ({ int t5 UNUSED; if (0) undef (); 0; });

  f4 ();

  undef ();
  f3 ();

  return;
}

int
main ()
{
    int foo = 1;
    f4 ();
}
