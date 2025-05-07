/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -march=z13" } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** test_le:
**     vl	(%v.),0\(%r2\),3
**     vl	(%v.),0\(%r3\),3
**     vecg	\2,\1
**     jne	\.L.+
**     vchlgs	%v.,\1,\2
**     lghi	%r2,0
**     locghinl	%r2,1
**     br	%r14
*/

int test_le (__int128 x, __int128 y) { return x <= y; }

/*
** test_leu:
**     vl	(%v.),0\(%r2\),3
**     vl	(%v.),0\(%r3\),3
**     veclg	\2,\1
**     jne	\.L.+
**     vchlgs	%v.,\1,\2
**     lghi	%r2,0
**     locghinl	%r2,1
**     br	%r14
*/

int test_leu (unsigned __int128 x, unsigned __int128 y) { return x <= y; }

/*
** test_lt:
**     vl	(%v.),0\(%r2\),3
**     vl	(%v.),0\(%r3\),3
**     vecg	\1,\2
**     jne	\.L.+
**     vchlgs	%v.,\2,\1
**     lghi	%r2,0
**     locghil	%r2,1
**     br	%r14
*/

int test_lt (__int128 x, __int128 y) { return x < y; }

/*
** test_ltu:
**     vl	(%v.),0\(%r2\),3
**     vl	(%v.),0\(%r3\),3
**     veclg	\1,\2
**     jne	\.L.+
**     vchlgs	%v.,\2,\1
**     lghi	%r2,0
**     locghil	%r2,1
**     br	%r14
*/

int test_ltu (unsigned __int128 x, unsigned __int128 y) { return x < y; }

/*
** test_ge:
**     vl	(%v.),0\(%r2\),3
**     vl	(%v.),0\(%r3\),3
**     vecg	\1,\2
**     jne	\.L.+
**     vchlgs	%v.,\2,\1
**     lghi	%r2,0
**     locghinl	%r2,1
**     br	%r14
*/

int test_ge (__int128 x, __int128 y) { return x >= y; }

/*
** test_geu:
**     vl	(%v.),0\(%r2\),3
**     vl	(%v.),0\(%r3\),3
**     veclg	\1,\2
**     jne	\.L.+
**     vchlgs	%v.,\2,\1
**     lghi	%r2,0
**     locghinl	%r2,1
**     br	%r14
*/

int test_geu (unsigned __int128 x, unsigned __int128 y) { return x >= y; }

/*
** test_gt:
**     vl	(%v.),0\(%r2\),3
**     vl	(%v.),0\(%r3\),3
**     vecg	\2,\1
**     jne	\.L.+
**     vchlgs	%v.,\1,\2
**     lghi	%r2,0
**     locghil	%r2,1
**     br	%r14
*/

int test_gt (__int128 x, __int128 y) { return x > y; }

/*
** test_gtu:
**     vl	(%v.),0\(%r2\),3
**     vl	(%v.),0\(%r3\),3
**     veclg	\2,\1
**     jne	\.L.+
**     vchlgs	%v.,\1,\2
**     lghi	%r2,0
**     locghil	%r2,1
**     br	%r14
*/

int test_gtu (unsigned __int128 x, unsigned __int128 y) { return x > y; }

/* { dg-final { scan-assembler-times {vceqgs\t} 4 } } */
/* { dg-final { scan-assembler-times {locghie\t} 2 } } */
/* { dg-final { scan-assembler-times {locghine\t} 2 } } */

int test_eq (__int128 x, __int128 y) { return x == y; }

int test_equ (unsigned __int128 x, unsigned __int128 y) { return x == y; }

int test_ne (__int128 x, __int128 y) { return x != y; }

int test_neu (unsigned __int128 x, unsigned __int128 y) { return x != y; }
