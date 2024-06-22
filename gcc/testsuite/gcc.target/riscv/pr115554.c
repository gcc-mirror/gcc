/* { dg-do compile } */

extern
__attribute__((target("arch=+zba")))
__attribute__((target("arch=+zbb")))
void foo(void);

extern
__attribute__((target("arch=+zbb")))
__attribute__((target("arch=+zbb")))
void bar(void);

/* { dg-error "extension 'zbb' appear more than one time" "" { target *-*-* } 0 } */
