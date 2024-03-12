#define func(vol, op1, op2)	\
_Bool op1##_##op2##_##vol (int a, int b)	\
{					\
 vol int x = op_##op1(a, b);		\
 return op_##op2(x, a);			\
}

#define op_lt(a, b) ((a) < (b))
#define op_le(a, b) ((a) <= (b))
#define op_eq(a, b) ((a) == (b))
#define op_ne(a, b) ((a) != (b))
#define op_gt(a, b) ((a) > (b))
#define op_ge(a, b) ((a) >= (b))
#define op_min(a, b) ((a) < (b) ? (a) : (b))
#define op_max(a, b) ((a) > (b) ? (a) : (b))


#define funcs(a) \
 a(min,lt) \
 a(max,lt) \
 a(min,gt) \
 a(max,gt) \
 a(min,le) \
 a(max,le) \
 a(min,ge) \
 a(max,ge) \
 a(min,ne) \
 a(max,ne) \
 a(min,eq) \
 a(max,eq)

#define funcs1(a,b) \
func(,a,b) \
func(volatile,a,b)

funcs(funcs1)

#define test(op1,op2)       \
do {                            \
  if (op1##_##op2##_(x,y) != op1##_##op2##_volatile(x,y))       \
    __builtin_abort();                                                  \
} while(0);

int main()
{
  for(int x = -10; x < 10; x++)
    for(int y = -10; y < 10; y++)
    {
        funcs(test)
    }
}
