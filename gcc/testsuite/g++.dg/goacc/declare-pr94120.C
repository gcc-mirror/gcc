/* { dg-do compile }  */

/* PR middle-end/94120  */

int b[8];
#pragma acc declare create (b)
 
namespace my {
 int d[8] = { 1, 2, 3, 4, 5, 6, 7, 8 };
 #pragma acc declare copyin (d)
}

namespace outer {
  namespace inner {
    int e[8] = { 1, 2, 3, 4, 5, 6, 7, 8 };
    #pragma acc declare copyin (e)
  }
}

int f[8] = { 1, 2, 3, 4, 5, 6, 7, 8 };
namespace my {
 #pragma acc declare copyin (f)		/* { dg-error "'f' must be a variable declared in the same scope as '#pragma acc declare'" }  */
}

namespace outer {
  int g[8] = { 1, 2, 3, 4, 5, 6, 7, 8 };
  namespace inner {
    #pragma acc declare copyin (g)	/* { dg-error "'outer::g' must be a variable declared in the same scope as '#pragma acc declare'" }  */
  }
}
