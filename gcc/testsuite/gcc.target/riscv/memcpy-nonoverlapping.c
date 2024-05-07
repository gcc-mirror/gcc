/* { dg-do compile } */
/* { dg-options "-mcpu=sifive-u74 -march=rv64gc -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Os" "-Oz" "-Og" } } */


#define COPY_N(N)				\
void copy##N (char *src, char *dst)		\
{						\
  dst = __builtin_assume_aligned (dst, 4096);	\
  src = __builtin_assume_aligned (src, 4096);	\
  __builtin_memcpy (dst, src, N);		\
}

/* Emits 1x {ld,sd} and 1x {lhu,lbu,sh,sb}.  */
COPY_N(11)

/* Emits 1x {ld,sd} and 1x {lw,lbu,sw,sb}.  */
COPY_N(13)

/* Emits 1x {ld,sd} and 1x {lw,lhu,sw,sh}.  */
COPY_N(14)

/* Emits 1x {ld,sd} and 1x {lw,lhu,lbu,sw,sh,sb}.  */
COPY_N(15)

/* Emits 2x {ld,sd} and 1x {lhu,lbu,sh,sb}.  */
COPY_N(19)

/* Emits 2x {ld,sd} and 1x {lw,lhu,lbu,sw,sh,sb}.  */
COPY_N(23)

/* The by-pieces infrastructure handles up to 24 bytes.
   So the code below is emitted via cpymemsi/block_move_straight.  */

/* Emits 3x {ld,sd} and 1x {lhu,lbu,sh,sb}.  */
COPY_N(27)

/* Emits 3x {ld,sd} and 1x {lw,lbu,sw,sb}.  */
COPY_N(29)

/* Emits 3x {ld,sd} and 1x {lw,lhu,lbu,sw,sh,sb}.  */
COPY_N(31)

/* { dg-final { scan-assembler-times "ld\t" 17 } } */
/* { dg-final { scan-assembler-times "sd\t" 17 } } */

/* { dg-final { scan-assembler-times "lw\t" 6 } } */
/* { dg-final { scan-assembler-times "sw\t" 6 } } */

/* { dg-final { scan-assembler-times "lhu\t" 7 } } */
/* { dg-final { scan-assembler-times "sh\t" 7 } } */

/* { dg-final { scan-assembler-times "lbu\t" 8 } } */
/* { dg-final { scan-assembler-times "sb\t" 8 } } */
