/* { dg-do compile } */
/* { dg-options "-std=c23" } */

struct __attribute__((designated_init)) S {
  int a, b, c, d;
  unsigned char e[128];
};

struct S s = { .a = 1, .b =
#embed __FILE__ limit(128)	/* { dg-warning "positional initialization of field in 'struct' declared with 'designated_init' attribute" } */
};				/* { dg-message "near initialization" "" { target *-*-* } .-1 } */
