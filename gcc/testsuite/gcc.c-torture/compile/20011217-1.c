/* Test that the initializer of a compound literal is properly walked
   when tree inlining.  */
/* Origin: PR c/5105 from <aj@suse.de>.  */

typedef struct { long p; } pt;

inline pt f (pt _p)
{
  long p = _p.p;

  return (pt) { (p) };
}

static int mmap_mem (void)
{
  pt p;
  p = f (p);

  return 0;
}
