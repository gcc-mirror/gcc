/* When generating o32 MIPS PIC, main's $gp save slot was out of range
   of a single load instruction.  */
struct big { int i[sizeof (int) >= 4 && sizeof (void *) >= 4 ? 0x4000 : 4]; };
struct big gb;
int foo (struct big b, int x) { return b.i[x]; }
#if defined(STACK_SIZE) && STACK_SIZE <= 0x10000
int main (void) { return 0; }
#else
int main (void) { return foo (gb, 0) + foo (gb, 1); }
#endif
