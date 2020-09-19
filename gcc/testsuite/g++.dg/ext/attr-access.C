/* PR middle-end/92721 - checking ICE on attribute access redeclaration
   Test to verify the handling of attribute access combining multiple
   declarations of the same function.
   { dg-do compile }
   { dg-options "-O0 -Wall -ftrack-macro-expansion=0" } */

#define RO(...)  __attribute__ ((access (read_only, __VA_ARGS__)))
#define RW(...)  __attribute__ ((access (read_write, __VA_ARGS__)))
#define WO(...)  __attribute__ ((access (write_only, __VA_ARGS__)))

typedef __INT32_TYPE__ int32_t;

void RO (1) RO (1) rop1_ror2 (const int32_t*, const int32_t&);
void RO (1) RO (2) rop1_ror2 (const int32_t*, const int32_t&);
void RO (2) RO (1) rop1_ror2 (const int32_t*, const int32_t&);
void RO (2) RO (2) rop1_ror2 (const int32_t*, const int32_t&);

void RW (1) RW (1) rdwrp1_rdwrr2 (int32_t*, int32_t&);
void RW (1) RW (2) rdwrp1_rdwrr2 (int32_t*, int32_t&);
void RW (2) RW (1) rdwrp1_rdwrr2 (int32_t*, int32_t&);
void RW (2) RW (2) rdwrp1_rdwrr2 (int32_t*, int32_t&);

void WO (1) WO (1) wop1_wor2 (int32_t*, int32_t&);
void WO (1) WO (2) wop1_wor2 (int32_t*, int32_t&);
void WO (2) WO (1) wop1_wor2 (int32_t*, int32_t&);
void WO (2) WO (2) wop1_wor2 (int32_t*, int32_t&);


// Verify that everything works even with no optimization.

void call_rop1_ror2_O0 (void)
{
  const int32_t x[1] = { };

  rop1_ror2 (x, x[0]);
  rop1_ror2 (x, x[1]);            // { dg-warning "reading 4 bytes from a region of size 0" }
  rop1_ror2 (x + 1, x[0]);        // { dg-warning "reading 4 bytes from a region of size 0" }
}

void call_rdwrp1_rdwrr2_O0 (void)
{
  int32_t x[1] = { };

  rdwrp1_rdwrr2 (x, x[0]);
  rdwrp1_rdwrr2 (x, x[1]);        // { dg-warning "accessing 4 bytes in a region of size 0" }
  rdwrp1_rdwrr2 (x + 1, x[0]);    // { dg-warning "accessing 4 bytes in a region of size 0" }
}

void call_wop1_wor2_O0 (void)
{
  int32_t x[1];

  wop1_wor2 (x, x[0]);
  wop1_wor2 (x, x[1]);            // { dg-warning "writing 4 bytes into a region of size 0" }
  wop1_wor2 (x + 1, x[0]);        // { dg-warning "writing 4 bytes into a region of size 0" }
}


// Verify that everything still works with -O1.

#pragma GCC optimize "1"

void call_rop1_ror2_O1 (void)
{
  const int32_t x[1] = { 1 };
  const int32_t *p0 = x, &r0 = x[0];
  const int32_t *p1 = (const int32_t*)((const char*)p0 + 1);
  const int32_t &r2 = *(const int32_t*)((const char*)p1 + 1);

  rop1_ror2 (x, x[0]);
  rop1_ror2 (x, x[1]);            // { dg-warning "reading 4 bytes from a region of size 0" }
  rop1_ror2 (x + 1, x[0]);        // { dg-warning "reading 4 bytes from a region of size 0" }

  rop1_ror2 (p0, r0);
  rop1_ror2 (p0, r2);             // { dg-warning "reading 4 bytes from a region of size 2" }
  rop1_ror2 (p1, r0);             // { dg-warning "reading 4 bytes from a region of size 3" }
}

void call_rdwrp1_rdwrr2_O1 (void)
{
  int32_t x[1] = { };
  int32_t *p0 = x, &r0 = x[0];
  int32_t *p1 = (int32_t*)((char*)p0 + 1);
  int32_t &r2 = *(int32_t*)((char*)p1 + 1);

  rdwrp1_rdwrr2 (x, x[0]);
  rdwrp1_rdwrr2 (x, x[1]);        // { dg-warning "accessing 4 bytes in a region of size 0" }
  rdwrp1_rdwrr2 (x + 1, x[0]);    // { dg-warning "accessing 4 bytes in a region of size 0" }

  rdwrp1_rdwrr2 (p0, r0);
  rdwrp1_rdwrr2 (p0, r2);         // { dg-warning "accessing 4 bytes in a region of size 2" }
  rdwrp1_rdwrr2 (p1, r0);         // { dg-warning "accessing 4 bytes in a region of size 3" }
}

void call_wop1_wor2_O1 (void)
{
  int32_t x[1];
  int32_t *p0 = x, &r0 = x[0];
  int32_t *p1 = (int32_t*)((char*)p0 + 1);
  int32_t &r2 = *(int32_t*)((char*)p1 + 1);

  wop1_wor2 (x, x[0]);
  wop1_wor2 (x, x[1]);            // { dg-warning "writing 4 bytes into a region of size 0" }
  wop1_wor2 (x + 1, x[0]);        // { dg-warning "writing 4 bytes into a region of size 0" }

  wop1_wor2 (p0, r0);
  wop1_wor2 (p0, r2);             // { dg-warning "writing 4 bytes into a region of size 2" }
  wop1_wor2 (p1, r0);             // { dg-warning "writing 4 bytes into a region of size 3" }
}
