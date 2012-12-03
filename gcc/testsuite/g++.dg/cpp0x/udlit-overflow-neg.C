// { dg-options "-std=c++0x -Woverflow" }
// PR c++/52654

int
operator"" _w(unsigned long long)
{ return 0; }

int
operator"" _w(long double)
{ return 0.0L; }

int i = 12345678901234567890123456789012345678901234567890_w;
int j = 12345678901234567890123456789.012345678901234567890e+1234567890_w;
int k = 12345678901234567890123456789.012345678901234567890e-1234567890_w;

// { dg-warning "integer literal exceeds range of " "" { target *-*-* } 12 }
// { dg-warning "floating literal exceeds range of " "" { target *-*-* } 13 }
// { dg-warning "floating literal truncated to zero" "" { target *-*-* } 14 }
