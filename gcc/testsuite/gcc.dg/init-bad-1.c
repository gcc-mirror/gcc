/* Test diagnostics for various bad initializers.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

void f(void);
void g(void) = f; /* { dg-error "function 'g' is initialized like a variable" } */

void h(a)
     int a = 1; /* { dg-error "parameter 'a' is initialized" } */
{
  struct s x = { 0 }; /* { dg-error "variable 'x' has initializer but incomplete type" } */
  /* { dg-warning "excess elements|near init" "excess" { target *-*-* } 12 } */
  /* { dg-error "storage size" "size" { target *-*-* } 12 } */
}

char s[1] = "x";
char s1[1] = { "x" };
char t[1] = "xy"; /* { dg-warning "initializer-string for array of chars is too long" } */
char t1[1] = { "xy" }; /* { dg-warning "initializer-string for array of chars is too long" } */
char u[1] = { "x", "x" }; /* { dg-error "excess elements in char array initializer" } */
/* { dg-message "near init" "near" { target *-*-* } 21 } */

int i = { }; /* { dg-error "empty scalar initializer" } */
/* { dg-message "near init" "near" { target *-*-* } 24 } */

int j = { 1 };

int k = { 1, 2 }; /* { dg-warning "excess elements in scalar initializer" } */
/* { dg-message "near init" "near" { target *-*-* } 29 } */

int a1[1] = { [1] = 0 }; /* { dg-error "array index in initializer exceeds array bounds" } */
/* { dg-message "near init" "near" { target *-*-* } 32 } */
int a2[1] = { [-1] = 0 }; /* { dg-error "array index in initializer exceeds array bounds" } */
/* { dg-message "near init" "near" { target *-*-* } 34 } */
int a3[1] = { [0 ... 1] = 0 }; /* { dg-error "array index range in initializer exceeds array bounds" } */
/* { dg-message "near init" "near" { target *-*-* } 36 } */
int a4[2] = { [1 ... 0] = 0 }; /* { dg-error "empty index range in initializer" } */
/* { dg-message "near init" "near" { target *-*-* } 38 } */
int a5[2] = { [0 ... 2] = 0 }; /* { dg-error "array index range in initializer exceeds array bounds" } */
/* { dg-message "near init" "near" { target *-*-* } 40 } */
int a6[2] = { [-1 ... 1] = 0 }; /* { dg-error "array index in initializer exceeds array bounds" } */
/* { dg-message "near init" "near" { target *-*-* } 42 } */
int a7[] = { [-1 ... 1] = 0 }; /* { dg-error "array index in initializer exceeds array bounds" } */
/* { dg-message "near init" "near" { target *-*-* } 44 } */
