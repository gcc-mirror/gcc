/* PR/12292
   http://gcc.gnu.org/ml/gcc-patches/2003-10/msg00143.html  */

char flags;

int bug12292(int t)
{
	flags &= ~(1 << (t + 4));
}
