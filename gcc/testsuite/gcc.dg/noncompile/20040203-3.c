/* PR/11658  The error message was quite mysterious for gcc up to 3.4.x */
/* { dg-options "-std=gnu89" } */

struct s { char c[1]; };
struct s f(void);

int bug_if(void) { if (f().c) return 1; else return 0; }  /* { dg-error "array that cannot be converted" } */
int bug_while(void) { while (f().c); }  /* { dg-error "array that cannot be converted" } */
int bug_do_while(void) { do ; while (f().c); }  /* { dg-error "array that cannot be converted" } */
int bug_for(void) { for ( ; f().c; ) ; }  /* { dg-error "array that cannot be converted" } */
int bug_or(void) { return (f().c || 1); }  /* { dg-error "array that cannot be converted" } */
int bug_and(void) { return (f().c && 1); }  /* { dg-error "array that cannot be converted" } */
int bug_cond(void) { return (f().c ? 1 : 0); }  /* { dg-error "array that cannot be converted" } */
int bug_cond2(void) { return (f().c ? : 0); }  /* { dg-error "array that cannot be converted" } */
int bug_not(void) { return !f().c; }  /* { dg-error "wrong type argument to unary exclamation mark" } */
int bug_bool(void) { return (_Bool) f().c; }  /* { dg-error "array that cannot be converted" } */
