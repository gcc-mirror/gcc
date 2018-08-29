#define vector64 __attribute__((vector_size(8)))

main(){

 vector64 int  c;
vector64 int a = {1, -1};
vector64 int b = {2, -2};
c = -a + b*b*(-1LL);
/* c is now {-5, -3} */

 printf("result is %llx\n", (long long)c); 
}
