#define func(vol, op1, op2, op3)	\
_Bool op1##_##op2##_##op3##_##vol (int a, int b)	\
{					\
 vol _Bool x = op_##op1(a, b);		\
 vol _Bool y = op_##op2(a, b);		\
 return op_##op3(x, y);			\
}

#define op_lt(a, b) ((a) < (b))
#define op_le(a, b) ((a) <= (b))
#define op_eq(a, b) ((a) == (b))
#define op_ne(a, b) ((a) != (b))
#define op_gt(a, b) ((a) > (b))
#define op_ge(a, b) ((a) >= (b))
#define op_xor(a, b) ((a) ^ (b))


#define funcs(a) \
 a(lt,lt,ne) \
 a(lt,lt,eq) \
 a(lt,lt,xor) \
 a(lt,le,ne) \
 a(lt,le,eq) \
 a(lt,le,xor) \
 a(lt,gt,ne) \
 a(lt,gt,eq) \
 a(lt,gt,xor) \
 a(lt,ge,ne) \
 a(lt,ge,eq) \
 a(lt,ge,xor) \
 a(lt,eq,ne) \
 a(lt,eq,eq) \
 a(lt,eq,xor) \
 a(lt,ne,ne) \
 a(lt,ne,eq) \
 a(lt,ne,xor) \
  \
 a(le,lt,ne) \
 a(le,lt,eq) \
 a(le,lt,xor) \
 a(le,le,ne) \
 a(le,le,eq) \
 a(le,le,xor) \
 a(le,gt,ne) \
 a(le,gt,eq) \
 a(le,gt,xor) \
 a(le,ge,ne) \
 a(le,ge,eq) \
 a(le,ge,xor) \
 a(le,eq,ne) \
 a(le,eq,eq) \
 a(le,eq,xor) \
 a(le,ne,ne) \
 a(le,ne,eq) \
 a(le,ne,xor)  \
 \
 a(gt,lt,ne) \
 a(gt,lt,eq) \
 a(gt,lt,xor) \
 a(gt,le,ne) \
 a(gt,le,eq) \
 a(gt,le,xor) \
 a(gt,gt,ne) \
 a(gt,gt,eq) \
 a(gt,gt,xor) \
 a(gt,ge,ne) \
 a(gt,ge,eq) \
 a(gt,ge,xor) \
 a(gt,eq,ne) \
 a(gt,eq,eq) \
 a(gt,eq,xor) \
 a(gt,ne,ne) \
 a(gt,ne,eq) \
 a(gt,ne,xor) \
  \
 a(ge,lt,ne) \
 a(ge,lt,eq) \
 a(ge,lt,xor) \
 a(ge,le,ne) \
 a(ge,le,eq) \
 a(ge,le,xor) \
 a(ge,gt,ne) \
 a(ge,gt,eq) \
 a(ge,gt,xor) \
 a(ge,ge,ne) \
 a(ge,ge,eq) \
 a(ge,ge,xor) \
 a(ge,eq,ne) \
 a(ge,eq,eq) \
 a(ge,eq,xor) \
 a(ge,ne,ne) \
 a(ge,ne,eq) \
 a(ge,ne,xor)

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
