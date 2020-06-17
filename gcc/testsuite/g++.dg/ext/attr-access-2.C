/* PR c++/94098 - checking ICE on attribute access redeclaration
   { dg-do compile }
   { dg-options "-Wall" } */

#define RO(...)  __attribute__ ((access (read_only, __VA_ARGS__)))
#define RW(...)  __attribute__ ((access (read_write, __VA_ARGS__)))
#define WO(...)  __attribute__ ((access (write_only, __VA_ARGS__)))

typedef __INT32_TYPE__ int32_t;

int        rdwr1_2_3_4 (void*, void*, void*, void*);
int RW (1) rdwr1_2_3_4 (void*, void*, void*, void*);
int RW (2) rdwr1_2_3_4 (void*, void*, void*, void*);
int RW (3) rdwr1_2_3_4 (void*, void*, void*, void*);
int RW (4) rdwr1_2_3_4 (void*, void*, void*, void*);

extern int32_t x[1];

void call_rdwrp1_2_3_4 (void)
{
  rdwr1_2_3_4 (x, x, x, x);
  rdwr1_2_3_4 (x, x, x, x + 1);     // { dg-warning "\\\[-Wstringop-overflow" }
  rdwr1_2_3_4 (x, x, x + 1, x);     // { dg-warning "\\\[-Wstringop-overflow" }
  rdwr1_2_3_4 (x, x + 1, x, x);     // { dg-warning "\\\[-Wstringop-overflow" }
  rdwr1_2_3_4 (x + 1, x, x, x);     // { dg-warning "\\\[-Wstringop-overflow" }
}


int        rdwr4_3_2_1 (void*, void*, void*, void*);
int RW (4) rdwr4_3_2_1 (void*, void*, void*, void*);
int RW (3) rdwr4_3_2_1 (void*, void*, void*, void*);
int RW (2) rdwr4_3_2_1 (void*, void*, void*, void*);
int RW (1) rdwr4_3_2_1 (void*, void*, void*, void*);

void call_rdwr4_3_2_1 (void)
{
  rdwr4_3_2_1 (x, x, x, x);
  rdwr4_3_2_1 (x, x, x, x + 1);     // { dg-warning "\\\[-Wstringop-overflow" }
  rdwr4_3_2_1 (x, x, x + 1, x);     // { dg-warning "\\\[-Wstringop-overflow" }
  rdwr4_3_2_1 (x, x + 1, x, x);     // { dg-warning "\\\[-Wstringop-overflow" }
  rdwr4_3_2_1 (x + 1, x, x, x);     // { dg-warning "\\\[-Wstringop-overflow" }
}


int                             rdwrall (void*, void*, void*, void*);
int RW (1)                      rdwrall (void*, void*, void*, void*);
int RW (1) RW (2)               rdwrall (void*, void*, void*, void*);
int RW (1) RW (2) RW (3)        rdwrall (void*, void*, void*, void*);
int RW (1) RW (2) RW (3) RW (4) rdwrall (void*, void*, void*, void*);

void call_rdwrall (void)
{
  rdwrall (x, x, x, x);
  rdwrall (x, x, x, x + 1);         // { dg-warning "\\\[-Wstringop-overflow" }
  rdwrall (x, x, x + 1, x);         // { dg-warning "\\\[-Wstringop-overflow" }
  rdwrall (x, x + 1, x, x);         // { dg-warning "\\\[-Wstringop-overflow" }
  rdwrall (x + 1, x, x, x);         // { dg-warning "\\\[-Wstringop-overflow" }
}


// Verify the attribute is a part of the function's type.
typedef __typeof__ (rdwrall) F;

void call_fnptr_typeof (F *f)
{
  f (x, x, x, x);
  f (x, x, x, x + 1);               // { dg-warning "\\\[-Wstringop-overflow" }
  f (x, x, x + 1, x);               // { dg-warning "\\\[-Wstringop-overflow" }
  f (x, x + 1, x, x);               // { dg-warning "\\\[-Wstringop-overflow" }
  f (x + 1, x, x, x);               // { dg-warning "\\\[-Wstringop-overflow" }
}


// Verify the attribute is effective on a typedef.
typedef        void FWRall (void*, void*, void*, void*);
typedef RW (1) void FWRall (void*, void*, void*, void*);
typedef RW (2) void FWRall (void*, void*, void*, void*);
typedef RW (3) void FWRall (void*, void*, void*, void*);
typedef RW (4) void FWRall (void*, void*, void*, void*);

void call_fnptr (FWRall *f)
{
  f (x, x, x, x);
  f (x, x, x, x + 1);               // { dg-warning "\\\[-Wstringop-overflow" "pr94171" { xfail *-*-* } }
  f (x, x, x + 1, x);               // { dg-warning "\\\[-Wstringop-overflow" "pr94171" { xfail *-*-* } }
  f (x, x + 1, x, x);               // { dg-warning "\\\[-Wstringop-overflow" "pr94171" { xfail *-*-* } }
  f (x + 1, x, x, x);               // { dg-warning "\\\[-Wstringop-overflow" "pr94171" { xfail *-*-* } }
}
