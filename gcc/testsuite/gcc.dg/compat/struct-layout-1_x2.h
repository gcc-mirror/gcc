#undef F
#undef N
#undef B
#undef TX
#define F(n, x, v, w) 						\
  if (arg.x != s##n.x) FAIL (n, 30);
#define N(n, x)
#define B(n, x, v, w)						\
  if (arg.x != s##n.x) FAIL (n, 30);
#define TX(n, type, attrs, fields, ops) 			\
void checkx##n (type S##n arg)					\
{								\
  ops								\
}
