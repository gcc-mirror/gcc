/* PR c/78666 - conflicting attribute alloc_size accepted
   { dg-do compile }
   { dg-options "-Wall" } */

#define A(...) __attribute__ ((alloc_size (__VA_ARGS__)))

A (1) char* f2_1 (int, int);
A (1) A (1) char* f2_1 (int, int);

A (1) char* f2_1 (int, int);            // { dg-message "previous declaration here" }

A (2) char* f2_1 (int, int);            // { dg-warning "ignoring attribute 'alloc_size \\\(2\\\)' because it conflicts with previous 'alloc_size \\\(1\\\)'" }


A (2) char* f2_2 (int, int);
A (2) char* f2_2 (int, int);            // { dg-message "previous declaration here" }

A (1) char* f2_2 (int, int);            // { dg-warning "ignoring attribute 'alloc_size \\\(1\\\)' because it conflicts with previous 'alloc_size \\\(2\\\)'" }


A (1) char* f3_1 (int, int, int);
A (1) char* f3_1 (int, int, int);       // { dg-message "previous declaration here" }

A (2) char* f3_1 (int, int, int);       // { dg-warning "ignoring attribute 'alloc_size \\\(2\\\)' because it conflicts with previous 'alloc_size \\\(1\\\)'" }
A (3) char* f3_1 (int, int, int);       // { dg-warning "ignoring attribute 'alloc_size \\\(3\\\)' because it conflicts with previous 'alloc_size \\\(1\\\)'" }
A (1, 2) char* f3_1 (int, int, int);    // { dg-warning "ignoring attribute 'alloc_size \\\(1, 2\\\)' because it conflicts with previous 'alloc_size \\\(1\\\)'" }
A (1, 3) char* f3_1 (int, int, int);    // { dg-warning "ignoring attribute 'alloc_size \\\(1, 3\\\)' because it conflicts with previous 'alloc_size \\\(1\\\)'" }


typedef A (2, 3) char* F3_2_3 (int, int, int);
typedef A (2, 3) char* F3_2_3 (int, int, int);
typedef A (2, 3) A (2, 3) char* F3_2_3 (int, int, int);

typedef A (1) char* F3_2_3 (int, int, int);   // { dg-warning "ignoring attribute 'alloc_size \\\(1\\\)' because it conflicts with previous 'alloc_size \\\(2, 3\\\)'" }
