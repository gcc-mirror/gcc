/* { dg-lto-do link } */
/* { dg-lto-options { {-O2 -flto -flto-toplevel-asm-heuristics -flto-partition=1to1} {-O2 -flto -flto-toplevel-asm-heuristics -flto-partition=max} {-O2 -flto -flto-toplevel-asm-heuristics -flto-partition=cache}} } */

extern int use_statics ();

extern int asm_var;

int main() {
  return asm_var + use_statics ();
}
