/* On SPARC64/SPARC-V9 it fails at -O0 and -O1, except with -m32. */
/* { dg-xfail-if "PR target/9200" { "sparc64-*-*" "sparcv9-*-*" } { "-O0" "-O1" } { "-m32" } } */
/* On regular SPARC it doesn't fail, except with -m64 at -O0 and -O1. */
/* { dg-xfail-if "PR target/9200" { "sparc-*-*" } { "-m64 -O0" "-m64 -O1" } { "" } } */

#define vector64 __attribute__((vector_size(8)))

main(){

 vector64 int  c;
vector64 int a = {1, -1};
vector64 int b = {2, -2};
c = -a + b*b*(-1LL);
/* c is now {5, 3} */

 printf("result is %llx\n", (long long)c); 
}
