/* { dg-do compile } */
/* { dg-options "(-mips16) addressing=absolute" } */
/* { dg-skip-if "requires inlining" { *-*-* } { "-O0" } { "" } } */

static inline MIPS16 int i1 (void) { return 1; }
static inline NOMIPS16 int i2 (void) { return 2; }
static inline MIPS16 int i3 (void) { return 3; }
static inline NOMIPS16 int i4 (void) { return 4; }

int NOMIPS16 f1 (void) { return i1 (); }
int MIPS16 f2 (void) { return i2 (); }
int MIPS16 f3 (void) { return i3 (); }
int NOMIPS16 f4 (void) { return i4 (); }

/* { dg-final { scan-assembler "i1:" } } */
/* { dg-final { scan-assembler "i2:" } } */
/* { dg-final { scan-assembler-not "i3:" } } */
/* { dg-final { scan-assembler-not "i4:" } } */
/* { dg-final { scan-assembler "\tjal\ti1" } } */
/* { dg-final { scan-assembler "\tjal\ti2" } } */
/* { dg-final { scan-assembler-not "\tjal\ti3" } } */
/* { dg-final { scan-assembler-not "\tjal\ti4" } } */
