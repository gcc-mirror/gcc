/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

/* The psABI asks a function using a standard calling convention variant to
   append an ABI tag to its mangled name.  */

__attribute__((riscv_vls_cc)) void default_vls_cc () {}

__attribute__((riscv_vls_cc(32))) void vlen_32 () {}

__attribute__((riscv_vls_cc(128))) void vlen_128 () {}

__attribute__((riscv_vls_cc(16384))) void vlen_16384 () {}

namespace v64 { __attribute__((riscv_vls_cc(64))) void f () {} }
namespace v512 { __attribute__((riscv_vls_cc(512))) void f () {} }

typedef void vls_fn_t (void) __attribute__((riscv_vls_cc(256)));
vls_fn_t from_typedef;
void from_typedef () {}

__attribute__((abi_tag("user"))) __attribute__((riscv_vls_cc(128)))
void tagged () {}

__attribute__((abi_tag("user", "abc"), riscv_vls_cc(128)))
void tagged2 () {}

namespace ns {
  struct S {
    __attribute__((riscv_vls_cc(128))) void member () {}
  };
  void instantiate () { S ().member (); }
}

template <typename T> __attribute__((riscv_vls_cc(128))) void tpl (T) {}
template void tpl<int> (int);

template <typename T> __attribute__((riscv_vls_cc(128))) void tpl_nodep () {}
template void tpl_nodep<int> ();

/* extern "C" has no mangling, so no tag, but it still has to compile.  */
extern "C" __attribute__((riscv_vls_cc(128))) void cfn () {}

/* The VLA vector calling convention variant has no ABI tag.  */
__attribute__((riscv_vector_cc)) void vector_cc () {}

void plain () {}

/* { dg-final { scan-assembler "_Z14default_vls_ccB16riscv_vls_cc_128v:" } } */
/* { dg-final { scan-assembler "_Z7vlen_32B15riscv_vls_cc_32v:" } } */
/* { dg-final { scan-assembler "_Z8vlen_128B16riscv_vls_cc_128v:" } } */
/* { dg-final { scan-assembler "_Z10vlen_16384B18riscv_vls_cc_16384v:" } } */
/* { dg-final { scan-assembler "_ZN3v641fB15riscv_vls_cc_64Ev:" } } */
/* { dg-final { scan-assembler "_ZN4v5121fB16riscv_vls_cc_512Ev:" } } */
/* { dg-final { scan-assembler "_Z12from_typedefB16riscv_vls_cc_256v:" } } */
/* { dg-final { scan-assembler "_Z6taggedB16riscv_vls_cc_128B4userv:" } } */
/* { dg-final { scan-assembler "_Z7tagged2B3abcB16riscv_vls_cc_128B4userv:" } } */
/* { dg-final { scan-assembler "_ZN2ns1S6memberB16riscv_vls_cc_128Ev:" } } */
/* { dg-final { scan-assembler "_Z3tplB16riscv_vls_cc_128IiEvT_:" } } */
/* { dg-final { scan-assembler "_Z9tpl_nodepB16riscv_vls_cc_128IiEvv:" } } */
/* { dg-final { scan-assembler "_Z9vector_ccv:" } } */
/* { dg-final { scan-assembler "_Z5plainv:" } } */
/* { dg-final { scan-assembler "\ncfn:" } } */
