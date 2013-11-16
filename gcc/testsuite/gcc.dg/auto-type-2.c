/* Test __auto_type.  Test invalid uses.  */
/* { dg-do compile } */
/* { dg-options "" } */

__auto_type; /* { dg-error "empty declaration" } */
__auto_type *p = (int *) 0; /* { dg-error "plain identifier" } */
struct s0 { int i : 1; } x;
void f (void) { __auto_type v = x.i; } /* { dg-error "bit-field initializer" } */
__auto_type i; /* { dg-error "initialized data declaration" } */
__auto_type g { } /* { dg-error "initialized data declaration" } */
__auto_type a = 1, b = 2; /* { dg-error "single declarator" } */
__auto_type long e0 = 0; /* { dg-error "__auto_type" } */
__auto_type short e1 = 0; /* { dg-error "__auto_type" } */
__auto_type signed e2 = 0; /* { dg-error "__auto_type" } */
__auto_type unsigned e3 = 0; /* { dg-error "__auto_type" } */
__auto_type _Complex e4 = 0; /* { dg-error "__auto_type" } */
long __auto_type e5 = 0; /* { dg-error "__auto_type" } */
short __auto_type e6 = 0; /* { dg-error "__auto_type" } */
signed __auto_type e7 = 0; /* { dg-error "__auto_type" } */
unsigned __auto_type e8 = 0; /* { dg-error "__auto_type" } */
_Complex __auto_type e9 = 0; /* { dg-error "__auto_type" } */
int __auto_type e10 = 0; /* { dg-error "two or more data types" } */
__auto_type _Bool e11 = 0; /* { dg-error "two or more data types" } */
