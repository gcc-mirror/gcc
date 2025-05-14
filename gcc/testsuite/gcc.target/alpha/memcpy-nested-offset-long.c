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
    int32_t l[2];
    due d;
  }
uno;

void
memcpy_nested_offset_long (uno *u)
{
  u->d.t.v = u->v;
}

/* Expect assembly such as:

	ldq $4,0($16)
	ldq $3,8($16)
	ldq $2,16($16)
	srl $4,32,$7
	ldq $1,24($16)
	srl $3,32,$6
	stl $4,68($16)
	srl $2,32,$5
	stl $7,72($16)
	srl $1,32,$4
	stl $3,76($16)
	stl $6,80($16)
	stl $2,84($16)
	stl $5,88($16)
	stl $1,92($16)
	stl $4,96($16)

   that is with four quadword loads at offsets 0, 8, 16, 24 each and
   eight longword stores at offsets 68, 72, 76, 80, 84, 88, 92, 96 each.  */

/* { dg-final { scan-assembler-times "\\sldq\\s\\\$\[0-9\]+,0\\\(\\\$16\\\)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sldq\\s\\\$\[0-9\]+,8\\\(\\\$16\\\)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sldq\\s\\\$\[0-9\]+,16\\\(\\\$16\\\)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sldq\\s\\\$\[0-9\]+,24\\\(\\\$16\\\)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstl\\s\\\$\[0-9\]+,68\\\(\\\$16\\\)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstl\\s\\\$\[0-9\]+,72\\\(\\\$16\\\)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstl\\s\\\$\[0-9\]+,76\\\(\\\$16\\\)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstl\\s\\\$\[0-9\]+,80\\\(\\\$16\\\)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstl\\s\\\$\[0-9\]+,84\\\(\\\$16\\\)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstl\\s\\\$\[0-9\]+,88\\\(\\\$16\\\)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstl\\s\\\$\[0-9\]+,92\\\(\\\$16\\\)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstl\\s\\\$\[0-9\]+,96\\\(\\\$16\\\)\\s" 1 } } */
