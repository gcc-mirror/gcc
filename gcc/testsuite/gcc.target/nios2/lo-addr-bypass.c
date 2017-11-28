/* { dg-do compile } */
/* { dg-options "-O2 -march=r2 -mbypass-cache" } */
/* { dg-final { scan-assembler-times "addi\tr., r., %lo" 12 } } */
/* { dg-final { scan-assembler-not "ldw\t" } } */
/* { dg-final { scan-assembler-not "stw\t" } } */
/* { dg-final { scan-assembler-not "ldwio\tr., %lo" } } */
/* { dg-final { scan-assembler-not "stwio\tr., %lo" } } */

/* Check that we do not generate %lo addresses with R2 ldstio instructions.
   %lo requires a 16-bit relocation and on R2 these instructions only have a
   12-bit register offset.  */
#define TYPE int

struct ss
{
  TYPE x1,x2;
};

extern TYPE S1;
extern TYPE S2[];

extern struct ss S3;
extern struct ss S4[];

TYPE *addr1 (void) { return &S1; }
TYPE get1 (void) { return S1; }
void set1 (TYPE value) { S1 = value; }

TYPE *addr2 (int i) { return &(S2[i]); }
TYPE get2 (int i) { return S2[i]; }
void set2 (int i, TYPE value) { S2[i] = value; }

TYPE *addr3 (void) { return &(S3.x2); }
TYPE get3 (void) { return S3.x2; }
void set3 (TYPE value) { S3.x2 = value; }

TYPE *addr4 (int i) { return &(S4[i].x2); }
TYPE get4 (int i) { return S4[i].x2; }
void set4 (int i, TYPE value) { S4[i].x2 = value; }

