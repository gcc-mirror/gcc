/* CTF generation for btf_type_tag attribute.

   CTF does not encode these attributes and no CTF_K_TYPE_TAG record should be
   emitted; these records only exist internally to facilitate translation
   to BTF.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */

int * __attribute__((btf_type_tag ("some_tag"))) x;

typedef unsigned int __attribute__((btf_type_tag ("other_tag"))) uint;
const uint u;

/* Expect 5 CTF types: int, int*, unsigned int, typedef, const.  */
/* { dg-final { scan-assembler-times "ctt_info" 5 } } */
/* Ensure no CTF_K_TYPE_TAG record is emitted.  */
/* { dg-final { scan-assembler-not "\[\t \]0x40*\[\t \]+\[^\n\]*ctt_info" } } */
/* { dg-final { scan-assembler-not "\[\t \]0x42*\[\t \]+\[^\n\]*ctt_info" } } */
