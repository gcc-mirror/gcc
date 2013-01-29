/* PR tree-optimization/56094 */
/* { dg-do compile } */
/* { dg-options "-O2 -g -fdump-tree-optimized-lineno" } */

_Bool cond;

int
fn0 (unsigned char, unsigned long long, unsigned char,
     unsigned char, signed short, unsigned int,
     unsigned char *);

extern void fn3 (unsigned char, unsigned char, unsigned char, unsigned char,
		 unsigned char, unsigned char, unsigned char, unsigned short);
extern void fn7 (int);
extern void fn8 (int);

static __inline__ __attribute__ ((always_inline)) void
fn1 (unsigned char arg0, unsigned char arg1, unsigned char arg2,
     unsigned char arg3, unsigned char arg4, unsigned char arg5,
     unsigned short arg6)
{
  asm volatile ("" :: "g" ((unsigned long long) arg0), "g" (arg1),
		      "g" (arg2), "g" (arg3), "g" (arg4), "g" (arg5),
		      "g" (arg6));
  if (cond)
    {
      unsigned char loc0 = 0;
      fn3 (loc0, arg0, arg1, arg2, arg3, arg4, arg5, arg6);
    }
}

static __inline__ __attribute__ ((always_inline)) void
fn4 (unsigned int arg0, unsigned long long arg1)
{
  asm volatile ("" :: "g" (arg0), "g" (arg1));
}

static __inline__ __attribute__ ((always_inline)) void
fn5 (unsigned int arg0, unsigned char arg1, unsigned int arg2,
     unsigned char arg3)
{
  asm volatile ("" :: "g" (arg0), "g" (arg1),
		      "g" ((unsigned long long) arg2), "g" (arg3));
}

static __inline__ __attribute__ ((always_inline)) void
fn6 (unsigned long long arg0, unsigned char arg1,
     unsigned char arg2, signed short arg3,
     unsigned int arg4, unsigned char * arg5)
{
  asm volatile ("" :: "g" (arg0), "g" ((unsigned long long) arg1),
		      "g" ((unsigned long long) arg2), "g" (arg3),
		      "g" (arg4), "g" (arg5));
  if (cond)
    {
      unsigned char loc0 = 0;
      fn0 (loc0, arg0, arg1, arg2, arg3, arg4, arg5);
    }
}

unsigned char b[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 0xa };
unsigned int q = sizeof (b) / sizeof (b[0]);

void
foo ()
{
  int i;
  for (i = 1; i <= 50; i++)
    {
      fn6 (i + 0x1234, i + 1, i + 0xa, i + 0x1234, q, b);
      fn5 (i + 0xabcd, i << 1, i + 0x1234, i << 2);
      fn7 (i + 0xdead);
      fn8 (i + 0xdead);
      fn1 (i, i + 1, i + 2, i + 3, i + 4, i + 5, i << 10);
      fn4 (i + 0xfeed, i);
    }
}

/* Verify no statements get the location of the foo () decl.  */
/* { dg-final { scan-tree-dump-not " : 65:1\\\]" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
