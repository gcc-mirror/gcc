/* { dg-lto-do link } */
/* { dg-lto-options {{-O1 -fwhopr -fPIC}} } */
/* { dg-extra-ld-options {-shared -O0} } */

int foo(void) {
 int ret, i;
 for (i = 0; i < 1; i++)
   ret = 0;
 for (i = 0; i < 1; i++)
   ret = 1;
 return ret;
}
