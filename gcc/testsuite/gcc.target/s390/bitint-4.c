/* { dg-do compile { target bitint } } */
/* { dg-options "-march=z9-109 -O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Verify calling convention. */

struct s_bitint5 {
  short a;
  unsigned _BitInt(5) b;
  char c;
};

static_assert (sizeof (struct s_bitint5) == 4);

/*
** s_bitint5_call:
**         iilf	%r2,2758168
**         jg	s_bitint5@PLT
*/

void s_bitint5 (struct s_bitint5 x);
void s_bitint5_call (void) { s_bitint5 ((struct s_bitint5){42, 22wbu, 24}); }

struct s_bitint9 {
  short a;
  unsigned _BitInt(9) b;
};

static_assert (sizeof (struct s_bitint9) == 4);

/*
** s_bitint9_call:
**         iilf	%r2,2752934
**         jg	s_bitint9@PLT
*/

void s_bitint9 (struct s_bitint9 x);
void s_bitint9_call (void) { s_bitint9 ((struct s_bitint9){42, 422wbu}); }

struct s_bitint17 {
  int a;
  unsigned _BitInt(17) b;
};

static_assert (sizeof (struct s_bitint17) == 8);

/*
** s_bitint17_call:
**         llihl	%r2,42
**         oilf	%r2,108198
**         jg	s_bitint17@PLT
*/

void s_bitint17 (struct s_bitint17 x);
void s_bitint17_call (void) { s_bitint17 ((struct s_bitint17){42, 108198wbu}); }

struct s_bitint33 {
  unsigned _BitInt(33) b;
};

static_assert (sizeof (struct s_bitint33) == 8);

/*
** s_bitint33_call:
**         llihl	%r2,1
**         oilf	%r2,2795939494
**         jg	s_bitint33@PLT
*/

void s_bitint33 (struct s_bitint33 x);
void s_bitint33_call (void) { s_bitint33 ((struct s_bitint33){7090906790wbu}); }
