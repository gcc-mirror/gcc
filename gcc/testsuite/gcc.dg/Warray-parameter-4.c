/* PR c/50584 - No warning for passing small array to C99 static array
   declarator
   Verify warnings for multidimensional arrays, including mismatches
   in bounds of arrays of VLAs.  (Mismatches in variable bounds are
   diagnosed by -Wvla-parameter.)
   { dg-do compile }
   { dg-options "-Wall -Warray-parameter=2" } */

// Verify that equivalent forms don't tigger a warning.

typedef int IA1[1];
typedef int IA1x_[][1];
typedef int IA1x0[0][1];

void fia_x1 (int[][1]);
void fia_x1 (IA1[0]);
void fia_x1 (IA1x_);
void fia_x1 (IA1x0);
void fia_x1 (int[0][1]);
void fia_x1 (int[static 0][1]);

// Same as above one more time.
void fia_x1 (int[][1]);
void fia_x1 (IA1[0]);
void fia_x1 (IA1x_);
void fia_x1 (IA1x0);
void fia_x1 (int[0][1]);
void fia_x1 (int[static 0][1]);


void fia1x1 (int[1][1]);
void fia1x1 (int[1][1]);
void fia1x1 (int[static 1][1]);

void fia2x1 (int[2][1]);
void fia2x1 (int[2][1]);
void fia2x1 (int[static 2][1]);

void fia1x2 (int[1][2]);
void fia1x2 (int[1][2]);
void fia1x2 (int[static 1][2]);

void fia1x1_2x1 (int[1][1]);          // { dg-message "previously declared as 'int\\\[1]\\\[1]'" }
void fia1x1_2x1 (int[2][1]);          // { dg-warning "\\\[-Warray-parameter" }
void fia1x1_2x1 (int[static 1][1]);
void fia1x1_2x1 (int[static 2][1]);   // { dg-warning "\\\[-Warray-parameter" }


void fia2x1_1x1 (int[2][1]);          // { dg-message "previously declared as 'int\\\[2]\\\[1]'" }
void fia2x1_1x1 (int[1][1]);          // { dg-warning "\\\[-Warray-parameter" }
void fia2x1_1x1 (int[2][1]);
void fia2x1_1x1 (int[static 1][1]);   // { dg-warning "\\\[-Warray-parameter" }
void fia2x1_1x1 (int[static 2][1]);


extern int n1, n2;

void fca_xn1 (char[][n1]);
void fca_xn1 (char[0][n1]);
void fca_xn1 (char[static 0][n1]);

void fca1xn1_2xn1 (char[1][n1]);
void fca1xn1_2xn1 (char[2][n1]);        // { dg-warning "\\\[-Warray-parameter" }
void fca1xn1_2xn1 (char[1][n1]);
void fca1xn1_2xn1 (char[static 1][n1]);
void fca1xn1_2xn1 (char[static 2][n1]); // { dg-warning "\\\[-Warray-parameter" }


/* Exercise VLAs with a mismatch in the bound for an ordinary array.  */
void fvlax_y (int n, int[][n]);
void fvlax_y (int n, int[0][n]);
void fvlax_y (int n, int[1][n]);        // { dg-warning "argument 2 of type 'int\\\[1]\\\[n]' with mismatched bound" }
void fvlax_y (int n, int[2][n]);        // { dg-warning "argument 2 of type 'int\\\[2]\\\[n]' with mismatched bound" }
void fvlax_y (int n, int[3][n]);        // { dg-warning "argument 2 of type 'int\\\[3]\\\[n]' with mismatched bound" }

void fvlaxn_y (int n, int[][n]);
void fvlaxn_y (int n, int[0][n]);
void fvlaxn_y (int n, int[1][n]);       // { dg-warning "\\\[-Warray-parameter" }
void fvlaxn_y (int n, int[2][n]);       // { dg-warning "\\\[-Warray-parameter" }
void fvlaxn_y (int n, int[3][n]);       // { dg-warning "\\\[-Warray-parameter" }

void fvlaxx_y (int[][*]);
void fvlaxx_y (int[0][*]);
void fvlaxx_y (int[1][*]);              // { dg-warning "\\\[-Warray-parameter" }
void fvlaxx_y (int[2][*]);              // { dg-warning "\\\[-Warray-parameter" }
void fvlaxx_y (int[3][*]);              // { dg-warning "\\\[-Warray-parameter" }


// Verify an array of pointers to an array of function pointers.

void ffpa7_5 (void (* (* (* [7])[5])(void))(void));
// { dg-message "previously declared as 'void \\\(\\\* ?\\\(\\\* ?\\\(\\\*\\\[7]\\\)\\\[5]\\\)\\\(void\\\)\\\)\\\(void\\\)'" "note" { target *-*-* } .-1 }
void ffpa7_5 (void (* (* (* [6])[5])(void))(void));
// { dg-warning "argument 1 of type 'void \\\(\\\* ?\\\(\\\* ?\\\(\\\*\\\[6]\\\)\\\[5]\\\)\\\(void\\\)\\\)\\\(void\\\)' with mismatched bound" "" { target *-*-* } .-1 }
void ffpa7_5 (void (* (* (* [])[5])(void))(void));
// { dg-warning "argument 1 of type 'void \\\(\\\* ?\\\(\\\* ?\\\(\\\*\\\[]\\\)\\\[5]\\\)\\\(void\\\)\\\)\\\(void\\\)' with mismatched bound" "" { target *-*-* } .-1 }
void ffpa7_5 (void (* (* (* (*))[5])(void))(void));
// { dg-warning "argument 1 of type 'void \\\(\\\* ?\\\(\\\* ?\\\(\\\*\\\*\\\)\\\[5]\\\)\\\(void\\\)\\\)\\\(void\\\)' declared as a pointer" "" { target *-*-* } .-1 }

// Same as above but with array of pointers to a VLA of function pointers.
void ffpa7_n1 (void (* (* (* [7])[n1])(void))(void));
// { dg-message "previously declared as 'void \\\(\\\* ?\\\(\\\* ?\\\(\\\*\\\[7]\\\)\\\[n1]\\\)\\\(void\\\)\\\)\\\(void\\\)'" "note" { target *-*-* } .-1 }
void ffpa7_n1 (void (* (* (* [8])[n1])(void))(void));
// { dg-warning "argument 1 of type 'void \\\(\\\* ?\\\(\\\* ?\\\(\\\*\\\[8]\\\)\\\[n1]\\\)\\\(void\\\)\\\)\\\(void\\\)' with mismatched bound" "" { target *-*-* } .-1 }

void ffpa9_x (void (* (* (* [9])[*])(void))(void));
// { dg-message "previously declared as 'void \\\(\\\* ?\\\(\\\* ?\\\(\\\*\\\[9]\\\)\\\[\\\*]\\\)\\\(void\\\)\\\)\\\(void\\\)'" "pr?????" { xfail *-*-* } .-1 }
// { dg-message "previously declared as 'void \\\(\\\* ?\\\(\\\* ?\\\(\\\*\\\[9]\\\)\\\[0]\\\)\\\(void\\\)\\\)\\\(void\\\)'" "" { target *-*-* } .-2 }
void ffpa9_x (void (* (* (* [8])[*])(void))(void));
// { dg-warning "argument 1 of type 'void \\\(\\\* ?\\\(\\\* ?\\\(\\\*\\\[8]\\\)\\\[\\\*]\\\)\\\(void\\\)\\\)\\\(void\\\)' with mismatched bound" "pr?????" { xfail *-*-* } .-1 }
// { dg-warning "argument 1 of type 'void \\\(\\\* ?\\\(\\\* ?\\\(\\\*\\\[8]\\\)\\\[0]\\\)\\\(void\\\)\\\)\\\(void\\\)' with mismatched bound"  "" { target *-*-* } .-2 }

/* Verify a three-dimensional array of pointers to two-dimensional arrays
   of pointers to function pointers.  */

void ffpa7_5_3 (void (* (* (* (* [7])[5])[3])(void))(void));
// { dg-message "previously declared as 'void ?\\\(\\\* ?\\\(\\\* ?\\\(\\\* ?\\\(\\\* ?\\\[7]\\\)\\\[5]\\\)\\\[3]\\\)\\\(void\\\)\\\)\\\(void\\\)'" "note" { target *-*-* } .-1 }
void ffpa7_5_3 (void (* (* (* (* [1])[5])[3])(void))(void));
// { dg-warning "argument 1 of type 'void ?\\\(\\\* ?\\\(\\\* ?\\\(\\\* ?\\\(\\\* ?\\\[1]\\\)\\\[5]\\\)\\\[3]\\\)\\\(void\\\)\\\)\\\(void\\\)' with mismatched bound" "" { target *-*-* } .-1 }
