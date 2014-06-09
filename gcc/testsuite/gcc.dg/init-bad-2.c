/* Test diagnostics for various bad initializers.  Test cases with
   standard syntax with -pedantic.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -pedantic" } */

void f(void);
void g(void) = f; /* { dg-error "function 'g' is initialized like a variable" } */

void h(a)
     int a = 1; /* { dg-error "parameter 'a' is initialized" } */
{
  struct s x = { 0 }; /* { dg-error "variable 'x' has initializer but incomplete type" } */
  /* { dg-message "excess elements|near init" "near" { target *-*-* } 13 } */
  /* { dg-error "storage size" "size" { target *-*-* } 13 } */
}

char s[1] = "x";
char s1[1] = { "x" };
char t[1] = "xy"; /* { dg-warning "initializer-string for array of chars is too long" } */
char t1[1] = { "xy" }; /* { dg-warning "initializer-string for array of chars is too long" } */
char u[1] = { "x", "x" }; /* { dg-error "excess elements in char array initializer" } */
/* { dg-message "near init" "near" { target *-*-* } 22 } */

int j = { 1 };

int k = { 1, 2 }; /* { dg-warning "excess elements in scalar initializer" } */
/* { dg-message "near init" "near" { target *-*-* } 27 } */

int a1[1] = { [1] = 0 }; /* { dg-error "array index in initializer exceeds array bounds" } */
/* { dg-message "near init" "near" { target *-*-* } 30 } */
int a2[1] = { [-1] = 0 }; /* { dg-error "array index in initializer exceeds array bounds" } */
/* { dg-message "near init" "near" { target *-*-* } 32 } */
