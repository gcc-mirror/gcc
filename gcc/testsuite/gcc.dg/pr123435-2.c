/* { dg-do compile } */

enum E { E1 = -1, E2 = 0, E3 = 1 };
const volatile enum E i2;
extern int i2;			/* { dg-error "conflicting type qualifiers" } */

