/* { dg-lto-do link } */
/* { dg-lto-options {{-O2 -flto -flto-partition=1to1} } } */

void asm_fn();
void asm_fn_used();

asm(".global %cc0\n%cc0:" :: ":" (asm_fn));
asm(".global %cc0\n%cc0:" :: ":" (asm_fn_used));


__attribute__((noinline))
int privatized_fn(int v) { return v + v;}

extern void call_privatized_fn();

int main() {
  privatized_fn (0);
  call_privatized_fn ();
}
