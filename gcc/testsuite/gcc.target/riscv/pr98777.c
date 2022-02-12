/* { dg-do compile } */
/* { dg-options "-fstrict-aliasing" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

typedef struct {
  _Complex e;
  _Complex f;
  _Complex g;
  _Complex h;
  _Complex i;
  _Complex j;
  _Complex k;
  _Complex l;
  _Complex m;
  _Complex n;
  _Complex o;
  _Complex p;
} Scl16;

Scl16 g1sScl16, g2sScl16, g3sScl16, g4sScl16, g5sScl16, g6sScl16, g7sScl16,
    g8sScl16, g9sScl16, g10sScl16, g11sScl16, g12sScl16, g13sScl16, g14sScl16,
    g15sScl16, g16sScl16;

void testvaScl16();

void
testitScl16() {
  testvaScl16(g10sScl16, g11sScl16, g12sScl16, g13sScl16, g14sScl16, g1sScl16,
              g2sScl16, g3sScl16, g4sScl16, g5sScl16, g6sScl16, g7sScl16,
              g8sScl16, g9sScl16, g10sScl16, g11sScl16, g12sScl16, g13sScl16,
              g14sScl16, g15sScl16, g16sScl16);
}
