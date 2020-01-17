/* PR c/93132 - bogus 'attribute((access))' warning when size-index
   is specified
   { dg-do compile }
   { dg-options "-Wall" } */

void __attribute__ ((access (read_only, 1, 5)))
f (void*, int, int, int, int);   // { dg-message "previous declaration" }

void __attribute__ ((access (read_only, 1, 3)))
f (void*, int, int, int, int);   // { dg-warning "attribute 'access\\\(read_only, 1, 3\\\)' mismatched positional argument values 3 and 5" }

void __attribute__ ((access (read_only, 1, 4)))
f (void*, int, int, int, int);   // { dg-warning "attribute 'access\\\(read_only, 1, 4\\\)' mismatched positional argument values 4 and 5" }

void __attribute__ ((access (read_only, 1, 5)))
f (void*, int, int, int, int);
