/* This test code is included into pr92398.p9-.c and pr92398.p9+.c.
   The two files have the tests for the number of instructions generated for
   P9- versus P9+.

   store generates difference instructions as below:
   P9+: mtvsrdd;xxlnot;stxv.
   P8/P7/P6 LE: not;not;std;std.
   P8 BE: mtvsrd;mtvsrd;xxpermdi;xxlnor;stxvd2x.
   P7/P6 BE: std;std;addi;lxvd2x;xxlnor;stxvd2x.
   P9+ and P9- LE are expected, P6/P7/P8 BE are unexpected.  */

void
bar (__int128_t *dst, __int128_t src)
{
  *dst =  ~src;
}

