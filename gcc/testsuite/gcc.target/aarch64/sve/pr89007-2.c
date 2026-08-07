/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O -ftree-vectorize -march=armv8.2-a+sve -msve-vector-bits=512 --save-temps" } */
/* { dg-final { check-function-bodies "**" "" } } */

#define N 1024
unsigned char dst[N];
unsigned char in1[N];
unsigned char in2[N];

/*
**  foo: 
**	...
**	eor	z[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d
**	lsr	z[0-9]+\.b, z[0-9]+\.b, #1
**	and	z[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d
**	add	z[0-9]+\.b, z[0-9]+\.b, z[0-9]+\.b
**	...
*/
void
foo ()
{
  for( int x = 0; x < N; x++ )
    dst[x] = (in1[x] + in2[x]) >> 1;
}

/* { dg-final { scan-assembler-not {\tuunpklo\t} } } */
/* { dg-final { scan-assembler-not {\tuunpkhi\t} } } */
