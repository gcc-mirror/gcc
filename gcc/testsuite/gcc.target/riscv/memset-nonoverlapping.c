/* { dg-do compile } */
/* { dg-options "-mcpu=sifive-u74 -march=rv64gc -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Os" "-Oz" "-Og" } } */

#define ZERO_N(N)				\
void zero##N (char *dst)			\
{						\
  dst = __builtin_assume_aligned (dst, 4096);	\
  __builtin_memset (dst, 0, N);			\
}

/* Emits 1x sd and 1x {sh,sb}.  */
ZERO_N(11)

/* Emits 1x sd and 1x {sw,sb}.  */
ZERO_N(13)

/* Emits 1x sd and 1x {sw,sh}.  */
ZERO_N(14)

/* Emits 1x sd and 1x {sw,sh,sb}.  */
ZERO_N(15)

/* Emits 2x sd and 1x {sh,sb}.  */
ZERO_N(19)

/* Emits 2x sd and 1x {sw,sh,sb}.  */
ZERO_N(23)

/* The by-pieces infrastructure handles up to 24 bytes.
   So the code below is emitted via cpymemsi/block_move_straight.  */

/* Emits 3x sd and 1x {sh,sb}.  */
ZERO_N(27)

/* Emits 3x sd and 1x {sw,sb}.  */
ZERO_N(29)

/* Emits 3x sd and 1x {sw,sh,sb}.  */
ZERO_N(31)

/* { dg-final { scan-assembler-times "sd\t" 17 } } */
/* { dg-final { scan-assembler-times "sw\t" 6 } } */
/* { dg-final { scan-assembler-times "sh\t" 7 } } */
/* { dg-final { scan-assembler-times "sb\t" 8 } } */
