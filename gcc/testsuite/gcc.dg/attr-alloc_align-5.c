/* PR c/78666 - conflicting attribute alloc_size accepted
   { dg-do compile }
   { dg-options "-Wall" } */

#define A(pos) __attribute__ ((alloc_align (pos)))

A (1) char* f2_1 (int, int);
A (1) char* f2_1 (int, int);            // { dg-message "previous declaration here" }

A (2) char* f2_1 (int, int);            // { dg-warning "ignoring attribute 'alloc_align \\\(2\\\)' because it conflicts with previous 'alloc_align \\\(1\\\)'" }


A (2) char* f2_2 (int, int);
A (2) char* f2_2 (int, int);            // { dg-message "previous declaration here" }

A (1) char* f2_2 (int, int);            // { dg-warning "ignoring attribute 'alloc_align \\\(1\\\)' because it conflicts with previous 'alloc_align \\\(2\\\)'" }


A (1) char* f3_1 (int, int, int);
A (1) char* f3_1 (int, int, int);       // { dg-message "previous declaration here" }

A (2) char* f3_1 (int, int, int);       // { dg-warning "ignoring attribute 'alloc_align \\\(2\\\)' because it conflicts with previous 'alloc_align \\\(1\\\)'" }
A (3) char* f3_1 (int, int, int);       // { dg-warning "ignoring attribute 'alloc_align \\\(3\\\)' because it conflicts with previous 'alloc_align \\\(1\\\)'" }
