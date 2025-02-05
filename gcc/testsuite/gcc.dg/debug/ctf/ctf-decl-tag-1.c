/* CTF generation for btf_decl_tag attribute.

   CTF does not encode these attributes and no CTF_K_DECL_TAG record should be
   emitted; these records only exist internally to facilitate translation
   to BTF.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */

int x __attribute__((btf_decl_tag ("some_tag")));

struct S {
  int a;
  int b __attribute__((btf_decl_tag ("_b")));
  int c;
};

struct S some_S;

void
__attribute__((btf_decl_tag ("__func")))
foo (int *ptr __attribute__((btf_decl_tag ("w"))), int val)
{
  *ptr = val;
}

/* Expect 5 CTF types: int, struct, void, int*, void (int*, int).  */
/* { dg-final { scan-assembler-times "ctt_info" 5 } } */
/* Ensure no CTF_K_DECL_TAG record is emitted.  */
/* { dg-final { scan-assembler-not "\[\t \]0x3c*\[\t \]+\[^\n\]*ctt_info" } } */
/* { dg-final { scan-assembler-not "\[\t \]0x3e*\[\t \]+\[^\n\]*ctt_info" } } */
