/* { dg-do compile } */
/* { dg-options "-mbwx" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

unsigned short unaligned_src_hi[4];

void
memcpy_unaligned_dst_hi (void *dst)
{
  __builtin_memcpy (dst, unaligned_src_hi, 7);
}

/* { dg-final { scan-assembler-times "\\sldwu\\s" 3 } } */
/* { dg-final { scan-assembler-times "\\sldbu\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstb\\s" 7 } } */
/* { dg-final { scan-assembler-not "\\szapnot\\s" } } */
