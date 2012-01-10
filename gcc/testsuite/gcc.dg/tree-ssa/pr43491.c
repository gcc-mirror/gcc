/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-stats" } */

#define REGISTER register

#if defined __arm__
# define REG1 asm("r4")
#elif defined __i386__
# define REG1 asm("ebx")
#elif defined __mips__
# define REG1 asm("s0")
#elif defined __x86_64__
# define REG1 asm("rbp")
#else
# undef REGISTER
# define REGISTER
# define REG1
#endif

REGISTER long data_0 REG1;
long data_3; 

long foo(long data, long v)
{
	long i;
	long t, u;

	if (data)
		i = data_0 + data_3;
	else {
		v = 2;
		i = 5;
	}
	t = data_0 + data_3;
	u = i;
	return v * t * u;
}
/* We should not eliminate global register variable when it is the RHS of
   a single assignment.  */
/* { dg-final { scan-tree-dump-times "Eliminated: 2" 1 "pre" { target { arm-*-* i?86-*-* mips*-*-* x86_64-*-* } } } } */
/* { dg-final { scan-tree-dump-times "Eliminated: 3" 1 "pre" { target { ! { arm-*-* i?86-*-* mips*-*-* x86_64-*-* } } } } } */
/* { dg-final { cleanup-tree-dump "pre" } } */
