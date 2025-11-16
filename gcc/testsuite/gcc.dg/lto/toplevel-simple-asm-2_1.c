extern void static_asm_fn ();
extern int static_asm_var;
asm("static_asm_fn:");
asm("static_asm_var:");

extern int asm_var;
asm(".global asm_var\nasm_var:");

int use_statics () {
  static_asm_fn ();
  return static_asm_var;
}
