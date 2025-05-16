/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

typedef unsigned int __attribute__ ((mode (DI))) int64_t;
typedef unsigned int __attribute__ ((mode (SI))) int32_t;

typedef union
  {
    int32_t l[8];
  }
val;

typedef struct
  {
    int32_t l[2];
    val v;
  }
tre;

typedef struct
  {
    int32_t l[3];
    tre t;
  }
due;

typedef struct
  {
    val v;
    int64_t q;
    int32_t l[3];
    due d;
  }
uno;

void
memcpy_nested_offset_quad (uno *u)
{
  u->d.t.v = u->v;
}

/* Expect assembly such as:

	ldq $4,0($16)
	ldq $3,8($16)
	ldq $2,16($16)
	ldq $1,24($16)
	stq $4,72($16)
	stq $3,80($16)
	stq $2,88($16)
	stq $1,96($16)

   that is with four quadword loads at offsets 0, 8, 16, 24 each
   and four quadword stores at offsets 72, 80, 88, 96 each.  */

/* { dg-final { scan-assembler-times "\\sldq\\s\\\$\[0-9\]+,0\\\(\\\$16\\\)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sldq\\s\\\$\[0-9\]+,8\\\(\\\$16\\\)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sldq\\s\\\$\[0-9\]+,16\\\(\\\$16\\\)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sldq\\s\\\$\[0-9\]+,24\\\(\\\$16\\\)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstq\\s\\\$\[0-9\]+,72\\\(\\\$16\\\)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstq\\s\\\$\[0-9\]+,80\\\(\\\$16\\\)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstq\\s\\\$\[0-9\]+,88\\\(\\\$16\\\)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstq\\s\\\$\[0-9\]+,96\\\(\\\$16\\\)\\s" 1 } } */
