/* { dg-lto-do link } */
/* { dg-lto-options {{-O2 -flto -flto-partition=1to1} } } */

extern int use_statics ();

extern int asm_var;

int main() {
  return use_statics ();
}
