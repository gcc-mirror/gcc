/* { dg-require-weak "" } */
/* { dg-require-alias "" } */
#define ASMNAME(cname)  ASMNAME2 (__USER_LABEL_PREFIX__, cname)
#define ASMNAME2(prefix, cname) STRING (prefix) cname
#define STRING(x)    #x

extern inline int foo (void) { return 23; }
int bar (void) { return foo (); }
extern int foo (void) __attribute__ ((weak, alias ("xxx")));
int baz (void) { return foo (); }
int xxx(void) __asm__(ASMNAME ("xxx"));
int xxx(void) { return 23; }
