extern void asm_fn_used();

__attribute__((used))
void local_caller() {
  asm_fn_used();
}

__attribute__((noipa))
static void privatized_fn() { asm volatile ("");}
asm(".long privatized_fn");

void call_privatized_fn() { privatized_fn();}
