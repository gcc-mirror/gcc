
#define func_name(outer,inner,cst) outer##inner##_##cst
#define func_name_v(outer,inner,cst) outer##inner##_##cst##_v

#define func_decl(outer,inner,cst) \
int outer##inner##_##cst (int) __attribute__((noipa)); \
int outer##inner##_##cst (int a) { \
  return (a op_##inner cst) op_##outer a; \
} \
int outer##inner##_##cst##_v (int) __attribute__((noipa)); \
int outer##inner##_##cst##_v (volatile int a) { \
  return (a op_##inner cst) op_##outer a; \
}

#define functions_n(outer, inner) \
func_decl(outer,inner,0) \
func_decl(outer,inner,1) \
func_decl(outer,inner,2)

#define functions() \
functions_n(eq,eq) \
functions_n(eq,ne) \
functions_n(ne,eq) \
functions_n(ne,ne)

#define op_ne !=
#define op_eq ==

#define test(inner,outer,cst,arg) \
func_name_v (inner,outer,cst)(arg) != func_name(inner,outer,cst)(arg)

functions()

#define tests_n(inner,outer,arg) \
if (test(inner,outer,0,arg)) __builtin_abort(); \
if (test(inner,outer,1,arg)) __builtin_abort(); \
if (test(inner,outer,2,arg)) __builtin_abort();

#define tests(arg) \
tests_n(eq,eq,arg) \
tests_n(eq,ne,arg) \
tests_n(ne,eq,arg) \
tests_n(ne,ne,arg)


int main()
{
  for(int n = -1; n <= 2; n++) {
    tests(n)
  }
}
