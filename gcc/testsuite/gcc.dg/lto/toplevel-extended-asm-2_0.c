/* { dg-lto-do link } */
/* { dg-lto-options {{-O2 -flto -flto-partition=1to1} {-O2 -flto -flto-partition=max} {-O2 -flto -flto-partition=cache}} } */

extern int use_statics ();

extern int asm_var;

static int a;
asm (".local %cc0\n %cc0:" :: ":"(&a));

int main() {
  return a + use_statics ();
}
