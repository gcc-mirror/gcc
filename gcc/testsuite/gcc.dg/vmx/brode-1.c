#include <altivec.h>
 int  main( )
 {
    static int a[3][5][7];
    {
       vector signed int a4v;
       a4v = vec_ldl(0, &a[0][0][0]);
    }
    return 0;
 }
