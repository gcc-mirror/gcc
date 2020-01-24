static __inline__ __attribute__((always_inline)) void set_bit(int nr, volatile void * addr) 
{
	volatile unsigned char *b_addr;
	b_addr = (volatile unsigned char *)addr + ((nr >> 3) ^ 3);
	nr &= 7;
	if (__builtin_constant_p (nr))					  
	{								  
		switch(nr)						  
		{							  
		case 0:							  
			__asm__("bset #0,%0" :"+m"(*b_addr) :"m"(*b_addr));
			break;	     					  
		case 1:		     					  
			__asm__("bset #1,%0" :"+m"(*b_addr) :"m"(*b_addr));
			break;	     					  
		case 2:		     					  
			__asm__("bset #2,%0" :"+m"(*b_addr) :"m"(*b_addr));
			break;	     					  
		case 3:		     					  
			__asm__("bset #3,%0" :"+m"(*b_addr) :"m"(*b_addr));
			break;	     					  
		case 4:		     					  
			__asm__("bset #4,%0" :"+m"(*b_addr) :"m"(*b_addr));
			break;	     					  
		case 5:		     					  
			__asm__("bset #5,%0" :"+m"(*b_addr) :"m"(*b_addr));
			break;	     					  
		case 6:		     					  
			__asm__("bset #6,%0" :"+m"(*b_addr) :"m"(*b_addr));
			break;	     					  
		case 7:		     					  
			__asm__("bset #7,%0" :"+m"(*b_addr) :"m"(*b_addr));
			break;						  
		}							  
	}								  
	else								  
	{								  
		__asm__("bset %w1,%0"  :"+m"(*b_addr)  :"g"(nr),"m"(*b_addr));	/* { dg-error "invalid 'asm'" "" } */

	}								  
}

static __inline__ __attribute__((always_inline)) int test_bit(int nr, const volatile void * addr)
{
	return (*((volatile unsigned char *)addr + ((nr >> 3) ^ 3)) & (1UL << (nr & 7))) != 0;
}

struct a {
        unsigned long a;
};

void dummy(struct a *a, int b);

int ice_func(struct a *a, int b)
{
  int c,d;
  unsigned int e;

  for(c=0;c<b;c++) {
    for(d=b; d <= b; d++) {
      if (!test_bit(d, &e)) {
        dummy(a, d * a->a);
        dummy(a, d * a->a);
        set_bit(d, &e);
      }
    }
    dummy(a, d * a->a);
  }

  return 0;
}
