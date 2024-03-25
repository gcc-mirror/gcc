/* { dg-options "" } */

inline void __attribute__((always_inline))
sc_callee () [[arm::streaming_compatible]] {}

inline void __attribute__((always_inline))
s_callee () [[arm::streaming]] {}

inline void __attribute__((always_inline))
n_callee () {}

[[arm::locally_streaming]] inline void __attribute__((always_inline))
sc_ls_callee () [[arm::streaming_compatible]] {}

[[arm::locally_streaming]] inline void __attribute__((always_inline))
n_ls_callee () {}

inline void __attribute__((always_inline))
sc_asm_callee () [[arm::streaming_compatible]] { asm (""); }

inline void __attribute__((always_inline))
s_asm_callee () [[arm::streaming]] { asm (""); }

inline void __attribute__((always_inline))
n_asm_callee () { asm (""); } // { dg-error "inlining failed" }

[[arm::locally_streaming]] inline void __attribute__((always_inline))
sc_ls_asm_callee () [[arm::streaming_compatible]] { asm (""); }

[[arm::locally_streaming]] inline void __attribute__((always_inline))
n_ls_asm_callee () { asm (""); }

[[arm::locally_streaming]] void
n_ls_caller ()
{
  sc_callee ();
  s_callee ();
  n_callee ();
  sc_ls_callee ();
  n_ls_callee ();

  sc_asm_callee ();
  s_asm_callee ();
  n_asm_callee ();
  sc_ls_asm_callee ();
  n_ls_asm_callee ();
}
