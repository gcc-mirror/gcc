/* On SPARC64/SPARC-V9 it fails, except with -m32.  On regular SPARC
   it doesn't fail, except with -m64.  In other words, this test fails
   on 64-bit SPARC.  Unfortunately, there's no way to encode that
   information in the dg framework, so the test will XPASS on 32-bit
   SPARC.  */
/* h8300 does not have long long */
/* { dg-excess-errors "PR target/9200" { xfail "sparc*-*-*" "h8300-*-*" } } */

#define vector64 __attribute__((vector_size(8)))

main(){

 vector64 int  c;
vector64 int a = {1, -1};
vector64 int b = {2, -2};
c = -a + b*b*(-1LL);
/* c is now {5, 3} */

 printf("result is %llx\n", (long long)c); 
}
