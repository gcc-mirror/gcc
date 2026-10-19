/* { dg-lto-do link } */
/* { dg-lto-options { {-O2 -flto -flto-toplevel-asm-heuristics -flto-partition=1to1 -no-pie} {-O2 -flto -flto-toplevel-asm-heuristics -flto-partition=max -no-pie} {-O2 -flto -flto-toplevel-asm-heuristics -flto-partition=cache -no-pie}} } */
/* { dg-skip-if "SPARC relocation overflow" { sparc*-*-solaris2* && lp64 } } */
/* { dg-skip-if "no-pie unsupported" { *-*-darwin* } "" "" } */

void asm_fn();
void asm_fn_used();

#ifdef __ELF__
#ifdef __sparc__
#define TYPE_PFX "#"
#elif defined (__arm__) || defined (__aarch64__)
#define TYPE_PFX "%"
#else
#define TYPE_PFX "@"
#endif
#endif

#ifdef TYPE_PFX
asm(".global asm_fn\n\t.type asm_fn, " TYPE_PFX "function\nasm_fn:");
asm(".global asm_fn_used\n\t.type asm_fn_used, " TYPE_PFX "function\nasm_fn_used:");
#else
asm(".global asm_fn\nasm_fn:");
asm(".global asm_fn_used\nasm_fn_used:");
#endif


__attribute__((noinline))
int privatized_fn(int v) { return v + v;}

extern void call_privatized_fn();

int main() {
  privatized_fn (0);
  call_privatized_fn ();
}
