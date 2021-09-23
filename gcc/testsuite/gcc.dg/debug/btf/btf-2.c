/* Check the BTF header information.  */

/* { dg-do compile } */
/* { dg-options "-gbtf -dA" } */

/* { dg-final { scan-assembler-times "0xeb9f.*btf_magic" 1} } */
/* { dg-final { scan-assembler-times "0x1.*btf_version" 1 } } */
/* { dg-final { scan-assembler-times "0.*btf_flags" 1 } } */

int foo;
