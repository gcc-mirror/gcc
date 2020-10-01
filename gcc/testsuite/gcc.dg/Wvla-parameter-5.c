/* Verify that combinations of array type qualifiers render correctly.
   { dg-do compile }
   { dg-options "-Wvla-parameter" } */

extern int n1, n2;

void fcx_n1 (int [const][n1]);     // { dg-message "previously declared as 'int\\\[const]\\\[n1]' with bound 'n1'" "note" }
void fcx_n1 (int [const][n2]);     // { dg-warning "argument 1 of type 'int\\\[const]\\\[n2]' declared with mismatched bound 'n2'" }

/* The mismatch in the array bound should not be diagnosed without
   -Warray-parameter but the mismatch in the VLA should still be
   diagnosed.  */
void fc3_n1 (int [const 3][n1]);   // { dg-message "previously declared as 'int\\\[const 3]\\\[n1]' with bound 'n1'" "note" }
void fc3_n1 (int [const 5][n2]);   // { dg-warning "argument 1 of type 'int\\\[const 5]\\\[n2]' declared with mismatched bound 'n2'" }


void frx_n1 (int [restrict][n1]);  // { dg-message "previously declared as 'int\\\[restrict]\\\[n1]' with bound 'n1'" "note" }
void frx_n1 (int [restrict][n2]);  // { dg-warning "argument 1 of type 'int\\\[restrict]\\\[n2]' declared with mismatched bound 'n2'" }


void fvx_n2 (int [volatile][n2]);  // { dg-message "previously declared as 'int\\\[volatile]\\\[n2]' with bound 'n2'" "note" }
void fvx_n2 (int [volatile][n1]);  // { dg-warning "argument 1 of type 'int\\\[volatile]\\\[n1]' declared with mismatched bound 'n1'" }
