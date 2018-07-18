/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "addi\tr., r., %lo" 4 } } */
/* { dg-final { scan-assembler-times "ldw\tr., %lo" 4 } } */
/* { dg-final { scan-assembler-times "stw\tr., %lo" 4 } } */

/* Check that various address forms involving a symbolic constant
   with a possible constant offset and/or index register are optimized
   to generate a %lo relocation in the load/store instructions instead
   of a plain register indirect addressing mode.  */

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

