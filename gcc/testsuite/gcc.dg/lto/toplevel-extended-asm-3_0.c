/* { dg-lto-do link } */
/* { dg-lto-options {{-O2 -flto -flto-partition=1to1} } } */

static void asm_fn();
asm("%cc0:" :: ":"(&asm_fn));

static int asm_var;
asm("%cc0:" :: ":"(&asm_var));

int use_asm_var1 (int* var);
int use_asm_var2 (int** var);
void use_asm_fn (void (*fn) ());

int main () {
  use_asm_fn (asm_fn);
  int *asm_var_ptr = &asm_var;
  return use_asm_var2(&asm_var_ptr) + use_asm_var1(&asm_var);
}
