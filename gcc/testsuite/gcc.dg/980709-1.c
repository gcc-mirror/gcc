/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options -O2 } */

extern __inline__ int test_and_set_bit(int nr, volatile void * addr)
{
	int oldbit;
	__asm__ __volatile__( "" 
		"btsl %2,%1\n\tsbbl %0,%0"
		:"=r" (oldbit),"=m" (addr)
		:"ir" (nr));
	return oldbit;
}
struct buffer_head {
	unsigned long b_state;		 
};
extern void lock_buffer(struct buffer_head * bh)
{
	while (test_and_set_bit(2 , &bh->b_state))
		__wait_on_buffer(bh);
}
