#undef F
#undef N
#undef B
#undef TX

#ifdef SKIP_VA
const int test_va = 0;
#else
const int test_va = 1;
#endif

#define F(n, x, v, w) 						\
  if (p->x != arg.x) FAIL (n, 74);
#define N(n, x)
#define B(n, x, v, w) 						\
  if (p->x != arg.x) FAIL (n, 74);
#define TX(n, type, attrs, fields, ops) 			\
void								\
check##n##va (int z, ...)					\
{								\
  type S##n arg, *p;						\
  va_list ap;							\
  int i;							\
								\
  if (test_va)							\
    {								\
      va_start (ap, z);						\
      for (i = 0; i < 5; ++i)					\
	{							\
	  p = NULL;						\
	  switch ((z << 4) | i)					\
	    {							\
	    case 0x10:						\
	      if (va_arg (ap, double) != 1.0)			\
		FAIL (n, 70);					\
	      break;						\
	    case 0x12:						\
	      if (va_arg (ap, long long) != 2LL)		\
		FAIL (n, 71);					\
	      break;						\
	    case 0x22:						\
	      if (va_arg (ap, long double) != 2.0L)		\
		FAIL (n, 72);					\
	      break;						\
	    case 0x11:						\
	    case 0x20:						\
	    case 0x21:						\
	    case 0x24:						\
	      p = &s##n;					\
	      arg = va_arg (ap, type S##n);			\
	      break;						\
	    case 0x13:						\
	    case 0x14:						\
	    case 0x23:						\
	      p = &a##n[2];					\
	      arg = va_arg (ap, type S##n);			\
	      break;						\
	    default:						\
	      FAIL (n, 73);					\
	      break;						\
	    }							\
	  if (p)						\
	    {							\
	      ops						\
	    }							\
	}							\
      va_end (ap);						\
    }								\
}
