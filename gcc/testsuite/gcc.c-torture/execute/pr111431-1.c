int
foo (int a)
{
  int b = a == 0;
  return (a & b);
}

#define function(vol,cst) \
__attribute__((noipa)) \
_Bool func_##cst##_##vol(vol int a) \
{ \
  vol int b = a == cst; \
  return (a & b); \
}

#define funcdefs(cst) \
function(,cst) \
function(volatile,cst)

#define funcs(f) \
f(0) \
f(1) \
f(5)

funcs(funcdefs)

#define test(cst) \
do { \
 if(func_##cst##_(a) != func_##cst##_volatile(a))\
   __builtin_abort(); \
} while(0);
int main(void)
{
  for(int a = -10; a <= 10; a++)
   {
     funcs(test)
   }
}

