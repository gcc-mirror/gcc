/* PR middle-end/83859 - attribute to establish relation between parameters
   for buffer and its size
   Test to verify the handling of attribute read_only combining multiple
   declarations of the same function.
   { dg-do compile }
   { dg-options "-Wall -ftrack-macro-expansion=0" } */

#define RW(...)    __attribute__ ((access (read_write, __VA_ARGS__)))
#define WO(...)  __attribute__ ((access (write_only, __VA_ARGS__)))

int rdwr1_rdwr1 (void*, void*);
int RW (1) RW (1) rdwr1_rdwr1 (void*, void*);
int RW (2) RW (2) rdwr1_rdwr1 (void*, void*);
int RW (1) RW (2) rdwr1_rdwr1 (void*, void*);
int RW (2) RW (1) rdwr1_rdwr1 (void*, void*);

int frdwr1_wr1 (void*, void*);
int RW (1) WO (1) frdwr1_wr1 (void*, void*);    // { dg-warning "attribute 'access\\(write_only, 1\\)' mismatch with mode 'read_write'" }

int RW (1) grdwr1_wr1 (void*, void*);           // { dg-message "previous declaration here" }

int WO (1) grdwr1_wr1 (void*, void*);         // { dg-warning "attribute 'access\\(write_only, 1\\)' mismatch with mode 'read_write'" }


int RW (1) RW (1, 2) frdwr1_rdwr1_1 (void*, int);   // { dg-warning "attribute 'access\\(read_write, 1, 2\\)' positional argument 2 missing in previous designation" }

int RW (1, 2) RW (1) frdwr1_1_rdwr1 (void*, int);   // { dg-warning "attribute 'access\\(read_write, 1\\)' missing positional argument 2 provided in previous designation" }

int RW (1)    grdwr1_rdwr1_1 (void*, int);   // { dg-message "previous declaration here" }
int RW (1, 2) grdwr1_rdwr1_1 (void*, int);   // { dg-warning "attribute 'access\\(read_write, 1, 2\\)' positional argument 2 missing in previous designation" }


typedef int *P;

int RW(1) WO(3) RW(5) WO(7) RW(9) WO(11) RW(13) WO(15) frw1_w3_rw5_w7_rw9_wr11_rw13_w15 (P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, int);

int RW(1) WO(3) RW(5) WO(7) RW(9) WO(11) RW(13) WO(15) frw1_w3_rw5_w7_rw9_wr11_rw13_w15 (P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, int);

int WO(1) WO(3) RW(5) WO(7) RW(9) WO(11) RW(13) WO(15) frw1_w3_rw5_w7_rw9_wr11_rw13_w15 (P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, int);
// { dg-warning "attribute 'access\\(write_only, 1\\)' mismatch with mode 'read_write'" "1" { target *-*-* } .-1 }

int RW(1) RW(3) RW(5) WO(7) RW(9) WO(11) RW(13) WO(15) frw1_w3_rw5_w7_rw9_wr11_rw13_w15 (P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, int);
// { dg-warning "attribute 'access\\(read_write, 3\\)' mismatch with mode 'write_only'" "3" { target *-*-* } .-1 }

int RW(1) WO(3) WO(5) WO(7) RW(9) WO(11) RW(13) WO(15) frw1_w3_rw5_w7_rw9_wr11_rw13_w15 (P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, int);
// { dg-warning "attribute 'access\\(write_only, 5\\)' mismatch with mode 'read_write'" "5" { target *-*-* } .-1 }

int RW(1) WO(3) RW(5) RW(7) RW(9) WO(11) RW(13) WO(15) frw1_w3_rw5_w7_rw9_wr11_rw13_w15 (P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, int);
// { dg-warning "attribute 'access\\(read_write, 7\\)' mismatch with mode 'write_only'" "7" { target *-*-* } .-1 }

int RW(1) WO(3) RW(5) WO(7) WO(9) WO(11) RW(13) WO(15) frw1_w3_rw5_w7_rw9_wr11_rw13_w15 (P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, int);
// { dg-warning "attribute 'access\\(write_only, 9\\)' mismatch with mode 'read_write'" "9" { target *-*-* } .-1 }

int RW(1) WO(3) RW(5) WO(7) RW(9) RW(11) RW(13) WO(15) frw1_w3_rw5_w7_rw9_wr11_rw13_w15 (P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, int);
// { dg-warning "attribute 'access\\(read_write, 11\\)' mismatch with mode 'write_only'" "11" { target *-*-* } .-1 }

int RW(1) WO(3) RW(5) WO(7) RW(9) WO(11) WO(13) WO(15) frw1_w3_rw5_w7_rw9_wr11_rw13_w15 (P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, int);
// { dg-warning "attribute 'access\\(write_only, 13\\)' mismatch with mode 'read_write'" "13" { target *-*-* } .-1 }

int RW(1) WO(3) RW(5) WO(7) RW(9) WO(11) RW(13) RW(15) frw1_w3_rw5_w7_rw9_wr11_rw13_w15 (P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, int);
// { dg-warning "attribute 'access\\(read_write, 15\\)' mismatch with mode 'write_only'" "15" { target *-*-* } .-1 }
