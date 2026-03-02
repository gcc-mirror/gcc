/* { dg-lto-do link } */
/* { dg-lto-options {{-O2 -flto -flto-partition=1to1} } } */

void asm_fn();
void asm_fn_used();

#ifdef __ELF__
#ifdef __sparc__
#define TYPE_PFX "#"
#elif defined (__arm__) || defined (__aarch64__)
#define TYPE_PFX "%%"
#else
#define TYPE_PFX "@"
#endif

#define ASM_FUNCTION ".global %cc0\n\t.type %cc0, " TYPE_PFX "function\n%cc0:"
#else
#define ASM_FUNCTION ".global %cc0\n%cc0:"
#endif

asm(ASM_FUNCTION :: ":" (asm_fn));
asm(ASM_FUNCTION :: ":" (asm_fn_used));


__attribute__((noinline))
int privatized_fn(int v) { return v + v;}

extern void call_privatized_fn();

int main() {
  privatized_fn (0);
  call_privatized_fn ();
}
