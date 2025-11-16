/* { dg-lto-do link } */
/* { dg-lto-options { {-O2 -flto -flto-toplevel-asm-heuristics -flto-partition=1to1} {-O2 -flto -flto-toplevel-asm-heuristics -flto-partition=max} {-O2 -flto -flto-toplevel-asm-heuristics -flto-partition=cache}} } */

void asm_fn();
void asm_fn_used();

asm(".global asm_fn\nasm_fn:");
asm(".global asm_fn_used\nasm_fn_used:");


__attribute__((noinline))
int privatized_fn(int v) { return v + v;}

extern void call_privatized_fn();

int main() {
  privatized_fn (0);
  call_privatized_fn ();
}
