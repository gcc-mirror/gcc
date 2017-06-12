/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef struct {
  int v;
} S1;
S1 clearS1 () { S1 s1 = { 0 }; return s1; }
 
typedef struct {
  S1 s1[4];
} S2;
void clearS2 (__seg_gs S2* p, int n) {
  for (int i = 0; i < n; ++i)
    p->s1[i] = clearS1 ();
}
 
typedef struct {
  int pad;
  S2 s2;
} S3;
 
long int BASE;
 
void fn1(int n) {
  clearS2 (&(((__seg_gs S3*)(BASE))->s2), n);
}
