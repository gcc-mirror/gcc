extern void static_asm_fn ();
extern int static_asm_var;
#if __APPLE__
asm("_static_asm_fn:");
asm("_static_asm_var:");
#else
asm("static_asm_fn:");
asm("static_asm_var:");
#endif

extern int asm_var;
#if __APPLE__
asm(".globl _asm_var\n_asm_var:");
#else
asm(".global asm_var\nasm_var:");
#endif

int use_statics () {
  static_asm_fn ();
  return static_asm_var;
}
