/* PR c/100719 - missing -Wvla-parameter on a mismatch in second parameter
   { dg-do compile }
   { dg-options "-Wall" } */

typedef struct A1 { int i; } A1;
typedef struct A2 { int i; } A2;
typedef struct A3 { int i; } A3;

extern int n, n1, n2, n3;

void f2 (int, A1[n], A2[n]);
void f2 (int, A1[n], A2[n]);

void f2_x1 (int, A1[n],  A2[n]);        // { dg-note "previously declared as 'A1\\\[n]'" }
void f2_x1 (int, A1[n1], A2[n]);        // { dg-warning "argument 2 of type 'A1\\\[n1]' declared with mismatched bound 'n1'" }

void f2_x2 (int, A1[n], A2[n]);         // { dg-note "previously declared as 'A2\\\[n]'" }
void f2_x2 (int, A1[n], A2[n2]);        // { dg-warning "argument 3 of type 'A2\\\[n2]' declared with mismatched bound 'n2'" }


void f3 (int, A1[n], A2[n], A3[n]);
void f3 (int, A1[n], A2[n], A3[n]);

void f3_x1 (int, A1[n],  A2[n], A3[n]);
// { dg-note "previously declared as 'A1\\\[n]'" "note" { target *-*-* } .-1 }
void f3_x1 (int, A1[n1], A2[n], A3[n]);
// { dg-warning "argument 2 of type 'A1\\\[n1]' declared with mismatched bound 'n1'" "" { target *-*-* } .-1 }

void f3_x2 (int, A1[n], A2[n],  A3[n]);
// { dg-note "previously declared as 'A2\\\[n]'" "note" { target *-*-* } .-1 }
void f3_x2 (int, A1[n], A2[n2], A3[n]);
// { dg-warning "argument 3 of type 'A2\\\[n2]' declared with mismatched bound 'n2'" "" { target *-*-* } .-1 }

void f3_x3 (int, A1[n], A2[n], A3[n]);
// { dg-note "previously declared as 'A3\\\[n]'" "note" { target *-*-* } .-1 }
void f3_x3 (int, A1[n], A2[n], A3[n3]);
// { dg-warning "argument 4 of type 'A3\\\[n3]' declared with mismatched bound 'n3'" "" { target *-*-* } .-1 }


void g3_x1 (int, A1[n],  A2[*], A3[n]);
// { dg-note "previously declared as 'A1\\\[n]'" "note" { target *-*-* } .-1 }
void g3_x1 (int, A1[n1], A2[*], A3[n]);
// { dg-warning "argument 2 of type 'A1\\\[n1]' declared with mismatched bound 'n1'" "" { target *-*-* } .-1 }

void g3_x2 (int, A1[*], A2[n],  A3[n]);
// { dg-note "previously declared as 'A2\\\[n]'" "note" { target *-*-* } .-1 }
void g3_x2 (int, A1[*], A2[n2], A3[n]);
// { dg-warning "argument 3 of type 'A2\\\[n2]' declared with mismatched bound 'n2'" "" { target *-*-* } .-1 }

void g3_x3 (int, A1[*], A2[*], A3[n]);
// { dg-note "previously declared as 'A3\\\[n]'" "note" { target *-*-* } .-1 }
void g3_x3 (int, A1[*], A2[*], A3[n3]);
// { dg-warning "argument 4 of type 'A3\\\[n3]' declared with mismatched bound 'n3'" "" { target *-*-* } .-1 }


void h3_x1 (int, A1[n],  A2[ ], A3[n]);
// { dg-note "previously declared as 'A1\\\[n]'" "note" { target *-*-* } .-1 }
void h3_x1 (int, A1[n1], A2[ ], A3[n]);
// { dg-warning "argument 2 of type 'A1\\\[n1]' declared with mismatched bound 'n1'" "" { target *-*-* } .-1 }

void h3_x2 (int, A1[ ], A2[n],  A3[n]);
// { dg-note "previously declared as 'A2\\\[n]'" "note" { target *-*-* } .-1 }
void h3_x2 (int, A1[ ], A2[n2], A3[n]);
// { dg-warning "argument 3 of type 'A2\\\[n2]' declared with mismatched bound 'n2'" "" { target *-*-* } .-1 }

void h3_x3 (int, A1[ ], A2[ ], A3[n]);
// { dg-note "previously declared as 'A3\\\[n]'" "note" { target *-*-* } .-1 }
void h3_x3 (int, A1[ ], A2[ ], A3[n3]);
// { dg-warning "argument 4 of type 'A3\\\[n3]' declared with mismatched bound 'n3'" "" { target *-*-* } .-1 }

