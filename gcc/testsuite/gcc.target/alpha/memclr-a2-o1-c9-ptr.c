/* { dg-do compile } */
/* { dg-options "-mbwx -mno-safe-partial" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

typedef unsigned int __attribute__ ((mode (QI))) int08_t;
typedef unsigned int __attribute__ ((mode (HI))) int16_t;

typedef struct
  {
    int08_t v[9];
  }
s_t;

typedef union
  {
    struct
      {
	int08_t c[1];
	s_t s;
	int08_t d[6];
      };
    int16_t a;
  }
u_t;

void __attribute__ ((noinline))
memclr_a2_o1_c9 (u_t *u)
{
  u->s = (s_t) { 0 };
}

/* Expect assembly such as:

	ldq_u $2,9($16)
	stb $31,1($16)
	lda $3,2($16)
	ldq_u $1,2($16)
	mskqh $2,$3,$2
	stq_u $2,9($16)
	mskql $1,$3,$1
	stq_u $1,2($16)

   that is with a byte store at offset 1 and with two unaligned load/store
   pairs at offsets 2 and 9 each.  */

/* { dg-final { scan-assembler-times "\\sldq_u\\s\\\$\[0-9\]+,2\\\(\\\$16\\\)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sldq_u\\s\\\$\[0-9\]+,9\\\(\\\$16\\\)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstb\\s\\\$31,1\\\(\\\$16\\\)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstq_u\\s\\\$\[0-9\]+,2\\\(\\\$16\\\)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstq_u\\s\\\$\[0-9\]+,9\\\(\\\$16\\\)\\s" 1 } } */
