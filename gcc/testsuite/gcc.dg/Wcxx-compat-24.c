/* PR c/117178 */
/* { dg-do compile } */
/* { dg-options "-Wc++-compat -Wno-unterminated-string-initialization" } */

char a1[] = "a";
char a2[1] = "a";	/* { dg-warning "initializer-string for array of 'char' is too long for C\\\+\\\+" } */
char a2nonstring[1] __attribute__((nonstring)) = "a";	/* { dg-warning "initializer-string for array of 'char' is too long for C\\\+\\\+" } */
char a3[1] = "aa";	/* { dg-warning "initializer-string for array of 'char' is too long" } */
char a4[2] = "a";

struct has_str {
  int a;
  char str1[4];
  char str2[4];
  char str3[4];
  char str4[4];
  char tag1[4] __attribute__((nonstring));
  char tag2[4] __attribute__((nonstring));
  char tag3[4] __attribute__((nonstring));
  char tag4[4] __attribute__((nonstring));
  int b;
};

struct has_str foo = {
  .str1 = "111",
  .str2 = "2222",	/* { dg-warning "initializer-string for array of 'char' is too long for C\\\+\\\+" } */
  .str3 = "33333",	/* { dg-warning "initializer-string for array of 'char' is too long" } */
  .str4 = { '4', '4', '4', '4' },
  .tag1 = "AAA",
  .tag2 = "BBBB",	/* { dg-warning "initializer-string for array of 'char' is too long for C\\\+\\\+" } */
  .tag3 = "CCCCC",	/* { dg-warning "initializer-string for array of 'char' is too long" } */
  .tag4 = { 'D', 'D', 'D', 'D' }
};
