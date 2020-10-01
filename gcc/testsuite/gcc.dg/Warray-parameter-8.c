/* Verify that combinations of array type qualifiers render correctly.
   { dg-do compile }
   { dg-options "-Warray-parameter" } */

void fatm (int[_Atomic 1]);       // { dg-message "previously declared as 'int\\\[_Atomic 1]" }
void fatm (int[_Atomic 2]);       // { dg-warning "argument 1 of type 'int\\\[_Atomic 2]' with mismatched bound" }


void fcst (int[const 2]);         // { dg-message "previously declared as 'int\\\[const 2]" }
void fcst (int[const 3]);         // { dg-warning "argument 1 of type 'int\\\[const 3]' with mismatched bound" }


void frst (int[restrict 3]);      // { dg-message "previously declared as 'int\\\[restrict 3]" }
void frst (int[restrict 4]);      // { dg-warning "argument 1 of type 'int\\\[restrict 4]' with mismatched bound" }

void fvol (int[volatile 4]);      // { dg-message "previously declared as 'int\\\[volatile 4]" }
void fvol (int[volatile 5]);      // { dg-warning "argument 1 of type 'int\\\[volatile 5]' with mismatched bound" }


void fcr (int[const restrict 1]);   // { dg-message "previously declared as 'int\\\[\(const restrict|restrict const\) 1]" }
void fcr (int[restrict volatile 2]); // { dg-warning "argument 1 of type 'int\\\[\(restrict volatile|volatile restrict\) 2]' with mismatched bound" }
void fcr (int[const restrict volatile 3]);  // { dg-warning "argument 1 of type 'int\\\[const volatile restrict 3]' with mismatched bound" }


extern int n;

void fcx_n (int [const 1][n]);      // { dg-message "previously declared as 'int\\\[const 1]\\\[n]'" "note" }
void fcx_n (int [restrict 2][n]);   // { dg-warning "argument 1 of type 'int\\\[restrict 2]\\\[n]' with mismatched bound" }


extern int n1, n2;

/* The mismatch in the array bound should be diagnosed but the mismatch
   in the VLA should not be without -Wvla-parameter.  */
void fc3_n1 (int [const 3][n1]);   // { dg-message "previously declared as 'int\\\[const 3]\\\[n1]'" "note" }
void fc3_n1 (int [const 5][n2]);   // { dg-warning "argument 1 of type 'int\\\[const 5]\\\[n2]' with mismatched bound" }
