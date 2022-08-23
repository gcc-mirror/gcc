/* PR tree-optimization/97027 - missing warning on buffer overflow storing
   a larger scalar into a smaller array
   Verify overflow by vector stores.
   { dg-do compile }
   { dg-options "-O2 -Wno-psabi" } */

#define V(N) __attribute__ ((vector_size (N)))
#define C1 (VC1){ 0 }
#define C2 (VC2){ 0, 1 }
#define C4 (VC4){ 0, 1, 2, 3 }
#define C8 (VC8){ 0, 1, 2, 3, 4, 5, 6, 7 }
#define C16 (VC16){ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 }

typedef V (1) char VC1;
typedef V (2) char VC2;
typedef V (4) char VC4;
typedef V (8) char VC8;
typedef V (16) char VC16;

extern char a1[1], a2[2], a3[3], a4[4], a5[5], a6[6], a7[7], a8[8], a15[15];

extern VC1 c1;
extern VC2 c2;
extern VC4 c4;
extern VC8 c8;
extern VC16 c16;

extern VC1 fc1 (void);
extern VC2 fc2 (void);
extern VC4 fc4 (void);
extern VC8 fc8 (void);
extern VC16 fc16 (void);

void nowarn (void)
{
  *(VC1*)a1 = C1;
  *(VC2*)a2 = C2;
  *(VC4*)a4 = C4;
  *(VC4*)a5 = C4;
  *(VC4*)a6 = C4;
  *(VC4*)a7 = C4;
  *(VC8*)a8 = C8;
  *(VC8*)a15 = C8;
}

void warn_vec_lit (void)
{
  *(VC2*)a1 = C2;       // { dg-warning "writing 2 bytes into a region of size 1" }
  *(VC4*)a2 = C4;       // { dg-warning "writing 4 bytes into a region of size 2" }
  *(VC4*)a3 = C4;       // { dg-warning "writing 4 bytes into a region of size 3" }
  *(VC8*)a4 = C8;       // { dg-warning "writing 8 bytes into a region of size 4" }
  *(VC8*)a7 = C8;       // { dg-warning "writing 8 bytes into a region of size 7" }
  *(VC16*)a15 = C16;    // { dg-warning "writing 16 bytes into a region of size 15" }
}

void warn_vec_decl (void)
{
  *(VC2*)a1 = c2;       // { dg-warning "writing 2 bytes into a region of size 1" }
  *(VC4*)a2 = c4;       // { dg-warning "writing 4 bytes into a region of size 2" }
  *(VC4*)a3 = c4;       // { dg-warning "writing 4 bytes into a region of size 3" }
  *(VC8*)a4 = c8;       // { dg-warning "writing 8 bytes into a region of size 4" }
  *(VC8*)a7 = c8;       // { dg-warning "writing 8 bytes into a region of size 7" }
  *(VC16*)a15 = c16;    // { dg-warning "writing 16 bytes into a region of size 15" }
}

void warn_vec_parm (VC2 pc2, VC4 pc4, VC8 pc8, VC16 pc16)
{
  *(VC2*)a1 = pc2;      // { dg-warning "writing 2 bytes into a region of size 1" }
  *(VC4*)a2 = pc4;      // { dg-warning "writing 4 bytes into a region of size 2" }
  *(VC4*)a3 = pc4;      // { dg-warning "writing 4 bytes into a region of size 3" }
  *(VC8*)a4 = pc8;      // { dg-warning "writing 8 bytes into a region of size 4" }
  *(VC8*)a7 = pc8;      // { dg-warning "writing 8 bytes into a region of size 7" }
  *(VC16*)a15 = pc16;   // { dg-warning "writing 16 bytes into a region of size 15" }
}

void warn_vec_func (void)
{
  *(VC2*)a1 = fc2 ();   // { dg-warning "writing 2 bytes into a region of size 1" }
  *(VC4*)a2 = fc4 ();   // { dg-warning "writing 4 bytes into a region of size 2" }
  *(VC4*)a3 = fc4 ();   // { dg-warning "writing 4 bytes into a region of size 3" }
  *(VC8*)a4 = fc8 ();   // { dg-warning "writing 8 bytes into a region of size 4" }
  *(VC8*)a7 = fc8 ();   // { dg-warning "writing 8 bytes into a region of size 7" }
  *(VC16*)a15 = fc16 ();// { dg-warning "writing 16 bytes into a region of size 15" }
}
