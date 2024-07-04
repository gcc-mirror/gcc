#define vector64 __attribute__((vector_size(8)))

int main(void){

 vector64 int  c;
vector64 int a = {1, -1};
vector64 int b = {2, -2};
c = -a + b*b*(-1LL);
/* c is now {-5, -3} */

 __builtin_printf("result is %llx\n", (long long)c); 
}
