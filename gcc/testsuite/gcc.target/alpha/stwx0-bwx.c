/* { dg-do compile } */
/* { dg-options "-mbwx" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

typedef struct { short v __attribute__ ((packed)); } shortx;

void
stwx0 (shortx *p)
{
  p->v = 0;
}

/* Expect assembly such as:

	stb $31,0($16)
	stb $31,1($16)
 */

/* { dg-final { scan-assembler-times "\\sstb\\s\\\$31," 2 } } */
