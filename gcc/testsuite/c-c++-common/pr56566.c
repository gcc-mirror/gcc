/* PR c/56566 */
/* { dg-do compile } */
/* { dg-options "" } */

struct S1 { enum E1 { N1 = -1, Z1 = 0 } e : 1; };
struct S2 { enum E2 { N2 = -1 } e : 1; };
struct S3 { enum E3 { Z3 = 0 } e : 1; };
struct S4 { enum E4 { N4 = -2, Z4 = 1 } e : 2; };
struct S5 { enum E5 { N5 = -3, Z5 = 1 } e : 3; };
struct S6 { enum E6 { N6 = -2, Z6 = 1 } e : 1; }; // { dg-warning "too small|narrower" }
struct S7 { enum E7 { N7 = -3, Z7 = 1 } e : 2; }; // { dg-warning "too small|narrower" }
struct S8 { enum E8 { Z8 = 1 } e : 1; };
struct S9 { enum E9 { Z9 = 2 } e : 2; };
struct S0 { enum E0 { Z0 = 2 } e : 1; };	  // { dg-warning "too small|narrower" }
