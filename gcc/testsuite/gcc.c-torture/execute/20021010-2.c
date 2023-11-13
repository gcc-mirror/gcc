/* cse.c failure on x86 target.
   Contributed by Stuart Hastings 10 Oct 2002 <stuart@apple.com> */
#include <stdlib.h>

typedef signed short SInt16;

typedef struct {
    SInt16 minx;
    SInt16 maxx;
    SInt16 miny;
    SInt16 maxy;
} IOGBounds;

int expectedwidth = 50;

unsigned int *global_vramPtr = (unsigned int *)0xa000;

IOGBounds global_bounds = { 100, 150, 100, 150 };
IOGBounds global_saveRect = { 75, 175, 75, 175 };

int
main(void)
{
  unsigned int *vramPtr;
  int width;
  IOGBounds saveRect = global_saveRect;
  IOGBounds bounds = global_bounds;

  if (saveRect.minx < bounds.minx) saveRect.minx = bounds.minx;
  if (saveRect.maxx > bounds.maxx) saveRect.maxx = bounds.maxx;

  vramPtr = global_vramPtr + (saveRect.miny - bounds.miny) ;
  width = saveRect.maxx - saveRect.minx;

  if (width != expectedwidth)
    abort ();
  exit (0);
}
