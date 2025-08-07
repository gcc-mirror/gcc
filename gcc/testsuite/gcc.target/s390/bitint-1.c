/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -march=z9-109" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Verify calling convention. */

static_assert (sizeof (_BitInt(5)) == 1);
static_assert (sizeof (_BitInt(9)) == 2);
static_assert (sizeof (_BitInt(17)) == 4);
static_assert (sizeof (_BitInt(33)) == 8);

/*
** bitint5_zero_extend_call:
**         lghi	%r2,22
**         jg	bitint5_zero_extend@PLT
*/

void bitint5_zero_extend (unsigned _BitInt(5) x);
void bitint5_zero_extend_call (void) { bitint5_zero_extend (22wbu); }

/*
** bitint5_sign_extend_call:
**         lghi	%r2,-10
**         jg	bitint5_sign_extend@PLT
*/

void bitint5_sign_extend (_BitInt(5) x);
void bitint5_sign_extend_call (void) { bitint5_sign_extend (-10wb); }

/*
** bitint9_zero_extend_call:
**         lghi	%r2,422
**         jg	bitint9_zero_extend@PLT
*/

void bitint9_zero_extend (unsigned _BitInt(9) x);
void bitint9_zero_extend_call (void) { bitint9_zero_extend (422wbu); }

/*
** bitint9_sign_extend_call:
**         lghi	%r2,-90
**         jg	bitint9_sign_extend@PLT
*/

void bitint9_sign_extend (_BitInt(9) x);
void bitint9_sign_extend_call (void) { bitint9_sign_extend (-90wb); }

/*
** bitint17_zero_extend_call:
**         lgfi	%r2,108198
**         jg	bitint17_zero_extend@PLT
*/

void bitint17_zero_extend (unsigned _BitInt(17) x);
void bitint17_zero_extend_call (void) { bitint17_zero_extend (108198wbu); }

/*
** bitint17_sign_extend_call:
**         lghi	%r2,-22874
**         jg	bitint17_sign_extend@PLT
*/

void bitint17_sign_extend (_BitInt(17) x);
void bitint17_sign_extend_call (void) { bitint17_sign_extend (-22874wb); }

/*
** bitint33_zero_extend_call:
**         llihl	%r2,1
**         oilf	%r2,2795939494
**         jg	bitint33_zero_extend@PLT
*/

void bitint33_zero_extend (unsigned _BitInt(33) x);
void bitint33_zero_extend_call (void) { bitint33_zero_extend (7090906790wbu); }

/*
** bitint33_sign_extend_call:
**         lgfi	%r2,-1499027802
**         jg	bitint33_sign_extend@PLT
*/

void bitint33_sign_extend (_BitInt(33) x);
void bitint33_sign_extend_call (void) { bitint33_sign_extend (-1499027802wb); }
