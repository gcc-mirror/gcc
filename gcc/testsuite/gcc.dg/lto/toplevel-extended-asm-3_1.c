__attribute__((noinline))
int use_asm_var1 (int* var) {
  return *var;
}

__attribute__((noinline))
int use_asm_var2 (int** var) {
  return **var;
}

__attribute__((noinline))
void use_asm_fn (void (*fn) ()) {
  fn ();
}
