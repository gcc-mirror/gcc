/* PR target/89954 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

signed char ab;

short aw;

int al;

short sext_andbw (void) { return ab & -2; }
short sext_orbw (void) { return ab | -3; }
short sext_xorbw (void) { return ab ^ -4; }

int sext_andbl (void) { return ab & -2; }
int sext_orbl (void) { return ab | -3; }
int sext_xorbl (void) { return ab ^ -4; }

int sext_andwl (void) { return aw & -2; }
int sext_orwl (void) { return aw | -3; }
int sext_xorwl (void) { return aw ^ -4; }

#ifdef __x86_64__

long long sext_andbq (void) { return ab & -2; }
long long sext_orbq (void) { return ab | -3; }
long long sext_xorbq (void) { return ab ^ -4; }

long long sext_andwq (void) { return aw & -2; }
long long sext_orwq (void) { return aw | -3; }
long long sext_xorwq (void) { return aw ^ -4; }

long long sext_andlq (void) { return al & -2; }
long long sext_orlq (void) { return al | -3; }
long long sext_xorlq (void) { return al ^ -4; }

#endif

/* { dg-final { scan-assembler-times "movsbw" 3 } } */
/* { dg-final { scan-assembler-times "movsbl" 3 } } */
/* { dg-final { scan-assembler-times "movswl" 3 } } */

/* { dg-final { scan-assembler-times "movsbq" 3 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "movswq" 3 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "movslq" 3 { target { ! ia32 } } } } */
