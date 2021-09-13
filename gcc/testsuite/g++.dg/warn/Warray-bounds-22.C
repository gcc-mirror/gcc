/* PR middle-end/99502 - missing -Warray-bounds on partial out of bounds
   access in C++
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

typedef __INT8_TYPE__  int8_t;
typedef __INT16_TYPE__ int16_t;
typedef __INT32_TYPE__ int32_t;

struct POD32
{
  int32_t i32;
};

int8_t a16[2];
int8_t apod32[sizeof (POD32)];

void nowarn_pod32_assign ()
{
  POD32 *p = (POD32*)apod32;
  *p = POD32 ();
}

void nowarn_pod32_assign_member ()
{
  POD32 *p = (POD32*)apod32;
  p->i32 = __LINE__;
}


void warn_pod32_assign ()
{
  POD32 *p = (POD32*)a16;
  *p = POD32 ();              // { dg-warning "-Warray-bounds" }
}

void warn_pod32_assign_member ()
{
  POD32 *p = (POD32*)a16;
  p->i32 = __LINE__;          // { dg-warning "-Warray-bounds" }
}


struct BV32                   // { dg-warning "-Warray-bounds" "due to pr99525" }
{
  int32_t i32;

  virtual ~BV32 ();
};

int8_t abv32[sizeof (BV32)];

void nowarn_bv32_assign ()
{
  BV32 *p = (BV32*)abv32;
  *p = BV32 ();
}

void nowarn_bv32_assign_member ()
{
  BV32 *p = (BV32*)abv32;
  p->i32 = __LINE__;
}

void warn_bv32_assign ()
{
  BV32 *p = (BV32*)a16;
  *p = BV32 ();                // { dg-warning "-Warray-bounds" "pr99525" { xfail *-*-* } }
}

void warn_bv32_assign_member ()
{
  BV32 *p = (BV32*)a16;
  p->i32 = __LINE__;          // { dg-warning "-Warray-bounds" }
}


struct DV32: virtual BV32 { };

int8_t adv32[sizeof (DV32)];
int8_t adv32_m1[sizeof adv32 - 1];

void nowarn_dv32_assign ()
{
  DV32 *p = (DV32*)adv32;
  *p = DV32 ();
}

void nowarn_dv32_assign_member ()
{
  DV32 *p = (DV32*)adv32;
  p->i32 = __LINE__;
}

void warn_dv32_assign ()
{
  DV32 *p = (DV32*)adv32_m1;
  *p = DV32 ();                // { dg-warning "-Warray-bounds" "pr?????" { xfail *-*-* } }
}

void warn_dv32_assign_member ()
{
  DV32 *p = (DV32*)adv32_m1;
  p->i32 = __LINE__;          // { dg-warning "-Warray-bounds" "pr?????" { xfail *-*-* } }
}
