/* { dg-skip-if "" { *-*-* } { "-mflip-mips16" } { "" } } */
/* { dg-options "(-mips16)" } */

void f1 (void);
void __attribute__((mips16)) f1 (void) {} /* { dg-error "conflicting" } */

void __attribute__((mips16)) f2 (void);
void f2 (void) {} /* { dg-error "conflicting" } */

void f3 (void);
void __attribute__((nomips16)) f3 (void) {} /* { dg-error "conflicting" } */

void __attribute__((nomips16)) f4 (void);
void f4 (void) {} /* { dg-error "conflicting" } */

void __attribute__((mips16, nomips16)) f5 (void) {} /* { dg-error "cannot have both" } */
