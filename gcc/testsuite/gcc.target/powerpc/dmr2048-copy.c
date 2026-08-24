/* { dg-do compile } */
/* { dg-require-effective-target powerpc_future_compile_ok } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */

/* Verify that a 2048-bit __dmr2048 value is loaded from and stored to
   memory with the correct byte order.

   A __dmr2048 value is made of two 1024-bit TDO halves.  The first half
   holds the most significant bits, so in big endian mode it must be
   loaded from and stored to the lower address, while in little endian
   mode it must be loaded from and stored to the higher address.  The two
   halves are held in VSX registers 32..39 and 40..47 respectively.  */

typedef __dmr2048 qdo_t;

void
copy (qdo_t *dst, qdo_t *src)
{
  *dst = *src;
}

/* Eight vector-pair loads and eight vector-pair stores make up the copy.  */
/* { dg-final { scan-assembler-times {\mlxvp\M} 8 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M} 8 } } */

/* Little endian: the first half is at the higher address.  */
/* { dg-final { scan-assembler-times {\mlxvp\s+3[0-9],128\(4\)} 1 { target { powerpc64le-*-* } } } } */
/* { dg-final { scan-assembler-times {\mlxvp\s+3[0-9],224\(4\)} 1 { target { powerpc64le-*-* } } } } */
/* { dg-final { scan-assembler-times {\mlxvp\s+4[0-7],0\(4\)} 1 { target { powerpc64le-*-* } } } } */
/* { dg-final { scan-assembler-times {\mlxvp\s+4[0-7],96\(4\)} 1 { target { powerpc64le-*-* } } } } */
/* { dg-final { scan-assembler-times {\mstxvp\s+3[0-9],128\(3\)} 1 { target { powerpc64le-*-* } } } } */
/* { dg-final { scan-assembler-times {\mstxvp\s+4[0-7],0\(3\)} 1 { target { powerpc64le-*-* } } } } */

/* Big endian: the first half is at the lower address.  */
/* { dg-final { scan-assembler-times {\mlxvp\s+3[0-9],0\(4\)} 1 { target { ! powerpc64le-*-* } } } } */
/* { dg-final { scan-assembler-times {\mlxvp\s+3[0-9],96\(4\)} 1 { target { ! powerpc64le-*-* } } } } */
/* { dg-final { scan-assembler-times {\mlxvp\s+4[0-7],128\(4\)} 1 { target { ! powerpc64le-*-* } } } } */
/* { dg-final { scan-assembler-times {\mlxvp\s+4[0-7],224\(4\)} 1 { target { ! powerpc64le-*-* } } } } */
/* { dg-final { scan-assembler-times {\mstxvp\s+3[0-9],0\(3\)} 1 { target { ! powerpc64le-*-* } } } } */
/* { dg-final { scan-assembler-times {\mstxvp\s+4[0-7],128\(3\)} 1 { target { ! powerpc64le-*-* } } } } */
