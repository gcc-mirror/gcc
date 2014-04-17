/* { dg-do compile }  */
/* { dg-options "-Wformat -Wformat-signedness" }  */

/* PR c/60194  */

void foo(unsigned u, int i, unsigned char uc, signed char sc) {
  __builtin_printf("%d\n", u);  /* { dg-warning "expects argument of type 'int', but argument 2 has type 'unsigned int'" } */
  __builtin_printf("%u\n", i);  /* { dg-warning "expects argument of type 'unsigned int', but argument 2 has type 'int'" } */
  __builtin_printf("%c\n", sc);
  __builtin_printf("%c\n", uc);
}
