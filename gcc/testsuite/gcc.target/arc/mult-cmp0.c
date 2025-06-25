/* { dg-do compile } */
/* { dg-options "-O1" } */

/* mpy.f   r1,r0,r1
   mov_s   r0,5    ;3
   j_s.d   [blink]
   mov.ne  r0,r1 */
unsigned int
ubar (unsigned int a, unsigned int b)
{
	unsigned int c = a * b;
	if (c == 0)
	{
		return 5;
	}
	return c;
}

/*  mpy.f   r1,r0,r1
    mov_s   r0,5    ;3
    j_s.d   [blink]
    mov.ne  r0,r1 */
signed int
bar (signed int a, signed int b)
{
	signed int c = a * b;
	if (c == 0)
	{
		return 5;
	}
	return c;
}

/* mpy.f   0,r0,r1
   mov_s   r0,1    ;3
   j_s.d   [blink]
   mov.eq  r0,5 */
unsigned int
ufoo (unsigned int a, unsigned int b)
{
	if (a * b == 0)
	{
		return 5;
	}
	return 1;
}

/*  mpy.f   0,r0,r1
    mov_s   r0,1    ;3
    j_s.d   [blink]
    mov.eq  r0,5 */
unsigned int
foo (signed int a, signed int b)
{
	if (a * b == 0)
	{
		return 5;
	}
	return 1;
}

/* { dg-final { scan-assembler-times "mpy\\.f\\s+0" 2 } } */
/* { dg-final { scan-assembler-times "mov\\.ne\\s+" 2 } } */
/* { dg-final { scan-assembler-times "mpy\\.f\\s+r" 2 } } */
/* { dg-final { scan-assembler-times "mov\\.eq\\s+" 2 } } */

