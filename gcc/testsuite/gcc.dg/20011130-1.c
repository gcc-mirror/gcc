/* { dg-do compile } */
/* { dg-options "-O3 -g -finline-limit=100" } */

#define WORK(x, y) __asm__ ("" : "=r" (x) : "0" (x)); y += x + 26
#define SOME_WORK(x, y) WORK(x, y); WORK(x, y); WORK(x, y); WORK(x, y)
#define MORE_WORK(x, y) SOME_WORK(x, y); SOME_WORK(x, y); SOME_WORK(x, y)
#define EVEN_MORE_WORK(x, y) MORE_WORK(x, y); MORE_WORK(x, y); MORE_WORK(x, y)
#define LOTS_OF_WORK(x, y) EVEN_MORE_WORK(x, y); EVEN_MORE_WORK(x, y)

static int __attribute__((unused)) foo (int x)
{
  static inline int bar (int x)
    {
      int y;
      y = x;
      LOTS_OF_WORK(x, y);
      LOTS_OF_WORK(x, y);
      LOTS_OF_WORK(x, y);
      LOTS_OF_WORK(x, y);
      LOTS_OF_WORK(x, y);
      LOTS_OF_WORK(x, y);
      return y;
    }
  return bar(x);
}
