// { dg-do compile { target *-*-darwin* } }
// { dg-options { -Wunused-parameter } }
// Radar 4125055

void foo(int x) {
#pragma unused ( x )
}
