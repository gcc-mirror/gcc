/* { dg-lto-do link } */
/* { dg-require-effective-target pie } */
/* { dg-lto-options { { -O -flto -fpie } } } */
/* { dg-extra-ld-options { -r -nostdlib -flinker-output=nolto-rel } } */

enum a { b } register_dccp();
void c();
void __attribute__((noreturn)) exit_error(enum a d) {
  __builtin_va_list va;
  __builtin_va_end(va);
  if (d)
    c();
  c();
  __builtin_exit(1);
}
int main() { register_dccp(); }
