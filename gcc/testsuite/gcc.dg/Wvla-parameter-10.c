/* PR c/100719 - missing -Wvla-parameter on a mismatch in second parameter
   { dg-do compile }
   { dg-options "-Wall" } */

typedef struct A1 { int i; } A1;
typedef struct A2 { int i; } A2;
typedef struct A3 { int i; } A3;

void f2 (int n, A1[n], A2[n]);
void f2 (int n, A1[n], A2[n]);

void f2_x1 (int n, A1[n],     A2[n]);   // { dg-note "previously declared as 'A1\\\[n]' with bound argument 1" }
void f2_x1 (int n, A1[n + 1], A2[n]);   // { dg-warning "argument 2 of type 'A1\\\[n \\+ 1]' declared with mismatched bound 'n \\+ 1'" }

void f2_x2 (int n, A1[n], A2[n]);       // { dg-note "previously declared as 'A2\\\[n]' with bound argument 1" }
void f2_x2 (int n, A1[n], A2[n + 2]);   // { dg-warning "argument 3 of type 'A2\\\[n \\+ 2]' declared with mismatched bound 'n \\+ 2'" }


void f3 (int n, A1[n], A2[n], A3[n]);
void f3 (int n, A1[n], A2[n], A3[n]);

void f3_x1 (int n, A1[n],     A2[n], A3[n]);
// { dg-note "previously declared as 'A1\\\[n]' with bound argument 1" "note" { target *-*-* } .-1 }
void f3_x1 (int n, A1[n + 1], A2[n], A3[n]);
// { dg-warning "argument 2 of type 'A1\\\[n \\+ 1]' declared with mismatched bound 'n \\+ 1'" "" { target *-*-* } .-1 }

void f3_x2 (int n, A1[n], A2[n],     A3[n]);
// { dg-note "previously declared as 'A2\\\[n]' with bound argument 1" "note" { target *-*-* } .-1 }
void f3_x2 (int n, A1[n], A2[n + 2], A3[n]);
// { dg-warning "argument 3 of type 'A2\\\[n \\+ 2]' declared with mismatched bound 'n \\+ 2'" "" { target *-*-* } .-1 }

void f3_x3 (int n, A1[n], A2[n], A3[n]);
// { dg-note "previously declared as 'A3\\\[n]' with bound argument 1" "note" { target *-*-* } .-1 }
void f3_x3 (int n, A1[n], A2[n], A3[n + 3]);
// { dg-warning "argument 4 of type 'A3\\\[n \\+ 3]' declared with mismatched bound 'n \\+ 3'" "" { target *-*-* } .-1 }


void g3_x1 (int n, A1[n],     A2[*], A3[n]);
// { dg-note "previously declared as 'A1\\\[n]' with bound argument 1" "note" { target *-*-* } .-1 }
void g3_x1 (int n, A1[n + 1], A2[*], A3[n]);
// { dg-warning "argument 2 of type 'A1\\\[n \\+ 1]' declared with mismatched bound 'n \\+ 1'" "" { target *-*-* } .-1 }

void g3_x2 (int n, A1[*], A2[n],     A3[n]);
// { dg-note "previously declared as 'A2\\\[n]' with bound argument 1" "note" { target *-*-* } .-1 }
void g3_x2 (int n, A1[*], A2[n + 2], A3[n]);
// { dg-warning "argument 3 of type 'A2\\\[n \\+ 2]' declared with mismatched bound 'n \\+ 2'" "" { target *-*-* } .-1 }

void g3_x3 (int n, A1[*], A2[*], A3[n]);
// { dg-note "previously declared as 'A3\\\[n]' with bound argument 1" "note" { target *-*-* } .-1 }
void g3_x3 (int n, A1[*], A2[*], A3[n + 3]);
// { dg-warning "argument 4 of type 'A3\\\[n \\+ 3]' declared with mismatched bound 'n \\+ 3'" "" { target *-*-* } .-1 }


void h3_x1 (int n, A1[n],     A2[ ], A3[n]);
// { dg-note "previously declared as 'A1\\\[n]' with bound argument 1" "note" { target *-*-* } .-1 }
void h3_x1 (int n, A1[n + 1], A2[ ], A3[n]);
// { dg-warning "argument 2 of type 'A1\\\[n \\+ 1]' declared with mismatched bound 'n \\+ 1'" "" { target *-*-* } .-1 }

void h3_x2 (int n, A1[ ], A2[n],     A3[n]);
// { dg-note "previously declared as 'A2\\\[n]' with bound argument 1" "note" { target *-*-* } .-1 }
void h3_x2 (int n, A1[ ], A2[n + 2], A3[n]);
// { dg-warning "argument 3 of type 'A2\\\[n \\+ 2]' declared with mismatched bound 'n \\+ 2'" "" { target *-*-* } .-1 }

void h3_x3 (int n, A1[ ], A2[ ], A3[n]);
// { dg-note "previously declared as 'A3\\\[n]' with bound argument 1" "note" { target *-*-* } .-1 }
void h3_x3 (int n, A1[ ], A2[ ], A3[n + 3]);
// { dg-warning "argument 4 of type 'A3\\\[n \\+ 3]' declared with mismatched bound 'n \\+ 3'" "" { target *-*-* } .-1 }

