/* PR c/20000 We only want to warn if the function returns
   explicitly. We do not care about the return type.  */
/* { dg-do compile } */
/* { dg-options "" } */

int g(void) __attribute__((noreturn)); 
int g2(void) __attribute__((noreturn)); /* { dg-bogus ".noreturn. function returns non-void value" } */
void h(void) __attribute__((noreturn));


int g(void) {
  return 1; /* { dg-warning "function declared 'noreturn' has a 'return' statement" } */
}           /* { dg-warning "'noreturn' function does return" "" { target *-*-* } 12 } */

int g2(void) {
  h();
}

typedef int ft(void);
volatile ft vg;
volatile ft vg2;

int vg(void); 
int vg2(void); /* { dg-bogus ".noreturn. function returns non-void value" } */

int vg(void) {
  return 1; /* { dg-warning "function declared 'noreturn' has a 'return' statement" "" { target c } 27 } */
}           /* { dg-warning "'noreturn' function does return" "" { target c } 27  } */

int vg2(void) {
  h();
}
