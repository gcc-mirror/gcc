/* { dg-lto-do link } */
/* { dg-lto-options {{-O2 -flto -flto-partition=1to1} {-O2 -flto -flto-partition=max} {-O2 -flto -flto-partition=cache}} } */

extern int use_statics ();

extern int asm_var;

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

int main() {
  return a + use_statics ();
}
