/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "addi\tr., r., %lo" 4 } } */
/* { dg-final { scan-assembler-times "ldbu\tr., %lo" 4 } } */
/* { dg-final { scan-assembler-times "ldb\tr., %lo" 16 } } */
/* { dg-final { scan-assembler-times "stb\tr., %lo" 4 } } */

/* Check that various address forms involving a symbolic constant
   with a possible constant offset and/or index register are optimized
   to generate a %lo relocation in the load/store instructions instead
   of a plain register indirect addressing mode.  */
/* Note: get* uses ldhu but ext* uses ldh since TYPE is signed.  */

#define TYPE signed char

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

int extw1 (void) { return (int)(S1); }
int extw2 (int i) { return (int)(S2[i]); }
int extw3 (void) { return (int)(S3.x2); }
int extw4 (int i) { return (int)(S4[i].x2); }
unsigned int extwu1 (void) { return (unsigned int)(S1); }
unsigned int extwu2 (int i) { return (unsigned int)(S2[i]); }
unsigned int extwu3 (void) { return (unsigned int)(S3.x2); }
unsigned int extwu4 (int i) { return (unsigned int)(S4[i].x2); }

short exth1 (void) { return (short)(S1); }
short exth2 (int i) { return (short)(S2[i]); }
short exth3 (void) { return (short)(S3.x2); }
short exth4 (int i) { return (short)(S4[i].x2); }
unsigned short exthu1 (void) { return (unsigned short)(S1); }
unsigned short exthu2 (int i) { return (unsigned short)(S2[i]); }
unsigned short exthu3 (void) { return (unsigned short)(S3.x2); }
unsigned short exthu4 (int i) { return (unsigned short)(S4[i].x2); }

