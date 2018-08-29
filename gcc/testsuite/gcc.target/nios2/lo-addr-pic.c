/* { dg-do compile { target nios2-*-linux-gnu } } */
/* { dg-options "-O2 -fpic" } */
/* { dg-final { scan-assembler-not "ldw\tr., %lo" } } */
/* { dg-final { scan-assembler-not "stw\tr., %lo" } } */

/* Check that address transformations for symbolic constants do NOT
   apply to code compiled with -fPIC, which requires references to
   go through the GOT pointer (r22) instead.  */

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

