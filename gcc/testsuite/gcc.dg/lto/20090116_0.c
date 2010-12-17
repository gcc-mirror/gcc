/* { dg-lto-do link } */
/* { dg-lto-options {{-O1 -flto -flto-partition=1to1 -fPIC}} } */
/* { dg-extra-ld-options {-r -nostdlib -O0} } */

int foo(void) {
 int ret, i;
 for (i = 0; i < 1; i++)
   ret = 0;
 for (i = 0; i < 1; i++)
   ret = 1;
 return ret;
}
