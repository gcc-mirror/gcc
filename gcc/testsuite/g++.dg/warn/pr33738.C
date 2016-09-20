// { dg-do run }
// { dg-options "-O2 -Wtype-limits -fstrict-enums" }
extern void link_error (void);

enum Alpha {
 ZERO = 0, ONE, TWO, THREE
};

Alpha a2;

int m1 = -1;
int GetM1() {
 return m1;
}

int main() {
 a2 = static_cast<Alpha>(GetM1());
 if (a2 == -1) {	// { dg-warning "always false due" "" { xfail *-*-* } } */
    link_error ();
 }
 a2 = static_cast<Alpha>(GetM1());
 if (-1 == a2) {	// { dg-warning "always false due" "" { xfail *-*-* } } */
    link_error ();
 }
 return 0;
}

