/* PR tree-optimization/97027 - missing warning on buffer overflow storing
   a larger scalar into a smaller array
   Verify overflow by aggregate stores.
   { dg-do compile }
   { dg-options "-O2" } */

#define A(N) (A ## N)
#define Ac1 (AC1){ 0 }
#define Ac2 (AC2){ 0, 1 }
#define Ac4 (AC4){ 0, 1, 2, 3 }
#define Ac8 (AC8){ 0, 1, 2, 3, 4, 5, 6, 7 }
#define Ac16 (AC16){ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 }

typedef struct AC1 { char a[1]; } AC1;
typedef struct AC2 { char a[2]; } AC2;
typedef struct AC3 { char a[3]; } AC3;
typedef struct AC4 { char a[4]; } AC4;
typedef struct AC5 { char a[5]; } AC5;
typedef struct AC8 { char a[8]; } AC8;
typedef struct AC16 { char a[16]; } AC16;

extern char a1[1], a2[2], a3[3], a4[4], a5[5], a6[6], a7[7], a8[8], a15[15];

extern AC1 ac1;
extern AC2 ac2;
extern AC4 ac4;
extern AC8 ac8;
extern AC16 ac16;

extern AC1 fac1 (void);
extern AC2 fac2 (void);
extern AC4 fac4 (void);
extern AC8 fac8 (void);
extern AC16 fac16 (void);

void nowarn (void)
{
  *(AC1*)a1 = Ac1;
  *(AC2*)a2 = Ac2;
  *(AC4*)a4 = Ac4;
  *(AC4*)a5 = Ac4;
  *(AC4*)a6 = Ac4;
  *(AC4*)a7 = Ac4;
  *(AC8*)a8 = Ac8;
  *(AC8*)a15 = Ac8;
}

void warn_comp_lit_zero (void)
{
  *(AC2*)a1 = (AC2){ }; // { dg-warning "writing 2 bytes into a region of size 1" }
  *(AC4*)a2 = (AC4){ }; // { dg-warning "writing 4 bytes into a region of size 2" }
  *(AC4*)a3 = (AC4){ }; // { dg-warning "writing 4 bytes into a region of size 3" }
  *(AC8*)a4 = (AC8){ }; // { dg-warning "writing 8 bytes into a region of size 4" }
  *(AC8*)a7 = (AC8){ }; // { dg-warning "writing 8 bytes into a region of size 7" }
  *(AC16*)a15 = (AC16){ };// { dg-warning "writing 16 bytes into a region of size 15" }
}

void warn_comp_lit (void)
{
  *(AC2*)a1 = Ac2;      // { dg-warning "writing 2 bytes into a region of size 1" "pr101475" { xfail *-*-* } }
  // After vectorization, below codes are optimized to
  // MEM <vector(4) char> [(char *)&a2] = { 0, 1, 2, 3 };
  // MEM <vector(4) char> [(char *)&a3] = { 0, 1, 2, 3 };
  // MEM <vector(8) char> [(char *)&a4] = { 0, 1, 2, 3, 4, 5, 6, 7 };
  // MEM <vector(8) char> [(char *)&a7] = { 0, 1, 2, 3, 4, 5, 6, 7 };
  // MEM <vector(16) char> [(char *)&a15] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
  // and warning should be expected, refer to PR102722.
  *(AC4*)a2 = Ac4;      // { dg-warning "writing 4 bytes into a region of size 2" "pr101475" { xfail { ! { vect_slp_v4qi_store } } } }
  *(AC4*)a3 = Ac4;      // { dg-warning "writing 4 bytes into a region of size 3" "pr101475" { xfail { ! { vect_slp_v4qi_store } } } }
  *(AC8*)a4 = Ac8;      // { dg-warning "writing 8 bytes into a region of size 4" "pr101475" { xfail { ! { vect_slp_v8qi_store } } } }
  *(AC8*)a7 = Ac8;      // { dg-warning "writing 8 bytes into a region of size 7" "pr101475" { xfail { ! { vect_slp_v8qi_store } } } }
  *(AC16*)a15 = Ac16;   // { dg-warning "writing 16 bytes into a region of size 15" "pr101475" { xfail { ! { vect_slp_v16qi_store } } } }
}

void warn_aggr_decl (void)
{
  *(AC2*)a1 = ac2;      // { dg-warning "writing 2 bytes into a region of size 1" }
  *(AC4*)a2 = ac4;      // { dg-warning "writing 4 bytes into a region of size 2" }
  *(AC4*)a3 = ac4;      // { dg-warning "writing 4 bytes into a region of size 3" }
  *(AC8*)a4 = ac8;      // { dg-warning "writing 8 bytes into a region of size 4" }
  *(AC8*)a7 = ac8;      // { dg-warning "writing 8 bytes into a region of size 7" }
  *(AC16*)a15 = ac16;   // { dg-warning "writing 16 bytes into a region of size 15" }
}

void warn_aggr_parm (AC2 pc2, AC4 pc4, AC8 pc8, AC16 pc16)
{
  *(AC2*)a1 = pc2;      // { dg-warning "writing 2 bytes into a region of size 1" }
  *(AC4*)a2 = pc4;      // { dg-warning "writing 4 bytes into a region of size 2" }
  *(AC4*)a3 = pc4;      // { dg-warning "writing 4 bytes into a region of size 3" }
  *(AC8*)a4 = pc8;      // { dg-warning "writing 8 bytes into a region of size 4" }
  *(AC8*)a7 = pc8;      // { dg-warning "writing 8 bytes into a region of size 7" }
  *(AC16*)a15 = pc16;   // { dg-warning "writing 16 bytes into a region of size 15" }
}

void warn_aggr_func (void)
{
  *(AC2*)a1 = fac2 ();  // { dg-warning "writing 2 bytes into a region of size 1" }
  *(AC4*)a2 = fac4 ();  // { dg-warning "writing 4 bytes into a region of size 2" }
  *(AC4*)a3 = fac4 ();  // { dg-warning "writing 4 bytes into a region of size 3" }
  *(AC8*)a4 = fac8 ();  // { dg-warning "writing 8 bytes into a region of size 4" }
  *(AC8*)a7 = fac8 ();  // { dg-warning "writing 8 bytes into a region of size 7" }
  *(AC16*)a15 = fac16 ();// { dg-warning "writing 16 bytes into a region of size 15" }

  extern AC2 fac2_x ();

  *(AC2*)a1 = fac2_x ();  // { dg-warning "writing 2 bytes into a region of size 1" }

  extern AC2 fac2_p (char*);

  *(AC2*)a1 = fac2_p (0); // { dg-warning "writing 2 bytes into a region of size 1" }
}
