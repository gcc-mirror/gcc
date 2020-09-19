/* PR c/50584 - No warning for passing small array to C99 static array
   declarator
   Verify that -Warray-parameter diagnoses mismatches in bounds of
   arrays between redeclarations of the same function and with pointer
   parameters pointing to those arrays.
   { dg-do compile }
   { dg-options "-Wall -Warray-parameter" } */

void fa_x (int (*)[]);        // { dg-message "previously declared as 'int \\\(\\\*\\\)\\\[]'" }
void fa_x (int (*)[2]);       // { dg-warning "\\\[-Warray-parameter" }
void fa_x (int (*)[2]);       // { dg-warning "mismatch in bound 1 of argument 1 declared as 'int \\\(\\\*\\\)\\\[2]'" }

void fa_2 (int (*)[2]);       // { dg-message "previously declared as 'int \\\(\\\*\\\)\\\[2]'" }
void fa_2 (int (*)[]);        // { dg-warning "mismatch in bound 1 of argument 1 declared as 'int \\\(\\\*\\\)\\\[]'" }
