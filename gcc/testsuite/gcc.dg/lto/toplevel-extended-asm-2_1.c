static void static_asm_fn ();
static int static_asm_var;
asm("%cc0:" :: ":" (&static_asm_fn));
asm("%cc0:" :: ":" (&static_asm_var));

extern int asm_var;
asm("%cc0:" :: ":" (&asm_var));

static int a;
#if __APPLE__
#if __LP64__
asm ("%cc0: .quad 0" :: ":"(&a));
#else
asm ("%cc0: .long 0" :: ":"(&a));
#endif
#else
asm (".local %cc0\n %cc0: .long 0" :: ":"(&a));
#endif

int use_statics () {
  static_asm_fn ();
  return static_asm_var + a;
}
