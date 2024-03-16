/* PR c/102989 */
/* { dg-do compile { target bitint575 } } */
/* { dg-options "-std=c23 -pedantic-errors" } */

enum A : _BitInt(15) { A0 = 0, A1 = 1 };		/* { dg-error "invalid 'enum' underlying type" } */
enum B : unsigned _BitInt(575) { B0 = 0, B1 = 1 };	/* { dg-error "invalid 'enum' underlying type" } */
enum C { C0 = 459875743wb, C1 = 3298437354uwb };
enum D { D0 = 61832600368276133515125630254911797508782837275302959978515764023224306276632966792579100265310761247399417856504034834837841258576687802491886538775473291979151693037174783wb, D1 };
/* { dg-error "enumerator value outside the range of 'intmax_t'" "" { target *-*-* } .-1 } */
/* { dg-error "overflow in enumeration values" "" { target *-*-* } .-2 } */
/* { dg-error "enumeration values exceed range of largest integer" "" { target *-*-* } .-3 } */
