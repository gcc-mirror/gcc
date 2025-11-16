static void static_asm_fn ();
static int static_asm_var;
asm("%cc0:" :: ":" (&static_asm_fn));
asm("%cc0:" :: ":" (&static_asm_var));

extern int asm_var;
asm("%cc0:" :: ":" (&asm_var));

int use_statics () {
  static_asm_fn ();
  return static_asm_var;
}
