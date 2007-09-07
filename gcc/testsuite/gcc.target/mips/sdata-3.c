/* { dg-mips-options "-G4 -mexplicit-relocs -mno-extern-sdata" } */

/* { dg-final { scan-assembler     "%gp_?rel\\(l4a\\)" } } */
/* { dg-final { scan-assembler     "%gp_?rel\\(l4b\\)" } } */
/* { dg-final { scan-assembler     "%gp_?rel\\(l4c\\)" } } */
/* { dg-final { scan-assembler-not "%gp_?rel\\(e4a\\)" } } */
/* { dg-final { scan-assembler     "%gp_?rel\\(e4b\\)" } } */
/* { dg-final { scan-assembler-not "%gp_?rel\\(c4\\)" } } */
/* { dg-final { scan-assembler     "%gp_?rel\\(g4a\\)" } } */
/* { dg-final { scan-assembler     "%gp_?rel\\(g4b\\)" } } */
/* { dg-final { scan-assembler     "%gp_?rel\\(g4c\\)" } } */

/* { dg-final { scan-assembler-not "%gp_?rel\\(l8a\\)" } } */
/* { dg-final { scan-assembler-not "%gp_?rel\\(l8b\\)" } } */
/* { dg-final { scan-assembler     "%gp_?rel\\(l8c\\)" } } */
/* { dg-final { scan-assembler-not "%gp_?rel\\(e8a\\)" } } */
/* { dg-final { scan-assembler     "%gp_?rel\\(e8b\\)" } } */
/* { dg-final { scan-assembler-not "%gp_?rel\\(c8\\)" } } */
/* { dg-final { scan-assembler-not "%gp_?rel\\(g8a\\)" } } */
/* { dg-final { scan-assembler-not "%gp_?rel\\(g8b\\)" } } */
/* { dg-final { scan-assembler     "%gp_?rel\\(g8c\\)" } } */

static int l4a;
static int l4b = 1;
static int __attribute__((section(".sdata"))) l4c;
extern int e4a;
extern int __attribute__((section(".sdata"))) e4b;
int __attribute__((common)) c4;
int __attribute__((nocommon)) g4a;
int g4b = 1;
int __attribute__((section(".sdata"))) g4c = 2;

static int l8a[2];
static int l8b[2] = { 1, 2 };
static int __attribute__((section(".sdata"))) l8c[2];
extern int e8a[2];
extern int __attribute__((section(".sdata"))) e8b[2];
int __attribute__((common)) c8[2];
int __attribute__((nocommon)) g8a[2];
int g8b[2] = { 1, 2 };
int __attribute__((section(".sdata"))) g8c[2] = { 1, 2 };

int f32a (void) { return l4a; }
int f32b (void) { return l4b; }
int f32c (void) { return l4c; }
int f32d (void) { return e4a; }
int f32e (void) { return e4b; }
int f32f (void) { return c4; }
int f32g (void) { return g4a; }
int f32h (void) { return g4b; }
int f32i (void) { return g4c; }

int f64a (void) { return l8a[0]; }
int f64b (void) { return l8b[0]; }
int f64c (void) { return l8c[0]; }
int f64d (void) { return e8a[0]; }
int f64e (void) { return e8b[0]; }
int f64f (void) { return c8[0]; }
int f64g (void) { return g8a[0]; }
int f64h (void) { return g8b[0]; }
int f64i (void) { return g8c[0]; }
