#define func(vol, op1, op2, op3)	\
_Bool op1##_##op2##_##op3##_##vol (int a, int b)	\
{					\
 vol _Bool x = op_##op1(a, b);		\
 vol _Bool y = op_##op2(a, b);		\
 return op_##op3(x - y, 0);		\
}

#define op_lt(a, b) ((a) < (b))
#define op_le(a, b) ((a) <= (b))
#define op_gt(a, b) ((a) > (b))
#define op_ge(a, b) ((a) >= (b))

#define funcs(a) \
 a(gt,lt,lt) \
 a(gt,lt,le) \
 a(gt,lt,gt) \
 a(gt,lt,ge) \
  \
 a(ge,le,lt) \
 a(ge,le,le) \
 a(ge,le,gt) \
 a(ge,le,ge) \
  \
 a(lt,gt,lt) \
 a(lt,gt,le) \
 a(lt,gt,gt) \
 a(lt,gt,ge) \
  \
 a(le,ge,lt) \
 a(le,ge,le) \
 a(le,ge,gt) \
 a(le,ge,ge) \

#define funcs1(a,b,c) \
func(,a,b,c) \
func(volatile,a,b,c)

funcs(funcs1)

#define test(op1,op2,op3)			\
do {						\
  if (op1##_##op2##_##op3##_(x,y)		\
      != op1##_##op2##_##op3##_volatile(x,y))	\
    __builtin_abort();				\
} while(0);

int main()
{
  for(int x = -10; x < 10; x++)
    for(int y = -10; y < 10; y++)
    {
        funcs(test)
    }
}
