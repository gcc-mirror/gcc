/* PR c/50584 - No warning for passing small array to C99 static array
   declarator
   { dg-do compile }
   { dg-options "-Wall" } */

typedef __INT16_TYPE__ int16_t;

void fa2 (int16_t[2]);
void fxa2 (int16_t[2]) __attribute__ ((nonnull));

void fas2 (int16_t[static 2]);

void fvla (unsigned n, int16_t[n]);
void fvlaS (unsigned n, int16_t[static n]);

void test_array_1_dim (void)
{
  int16_t a1[1];
  int16_t a2[2];
  int16_t i;

  fa2 (0);
  fa2 (a2);
  fa2 (a1);                   // { dg-warning "'fa2' accessing 4 bytes in a region of size 2 " }
  fa2 (&i);                   // { dg-warning "'fa2' accessing 4 bytes in a region of size 2 " }

  fxa2 (0);                   // { dg-warning "\\\[-Wnonnull" }
  fxa2 (a2);
  fxa2 (a1);                  // { dg-warning "'fxa2' accessing 4 bytes in a region of size 2 " }
  fxa2 (&i);                  // { dg-warning "'fxa2' accessing 4 bytes in a region of size 2 " }

  fas2 (0);                   // { dg-warning "\\\[-Wnonnull" }
  fas2 (a2);
  fas2 (a1);                  // { dg-warning "'fas2' accessing 4 bytes in a region of size 2 " }
  fas2 (&i);                  // { dg-warning "'fas2' accessing 4 bytes in a region of size 2 " }

  fvla (1, 0);
  fvlaS (1, 0);               // { dg-warning "\\\[-Wnonnull" }
  fvla (1, &i);
  fvla (2, a2);
  fvla (2, a1);               // { dg-warning "'fvla' accessing 4 bytes in a region of size 2 " }
  fvla (2, &i);               // { dg-warning "'fvla' accessing 4 bytes in a region of size 2 " }
}


void fac2 (const int16_t[2]);
void fxac2 (const int16_t[2]) __attribute__ ((nonnull));

void facs2 (const int16_t[static 2]);

void fvlac (unsigned n, const int16_t[n]);
void fvlacS (unsigned n, const int16_t[static n]);

void test_const_array_1_dim (void)
{
  int16_t a1[1];
  int16_t a2[2];
  int16_t i;

  fac2 (0);
  fac2 (a2);
  fac2 (a1);                  // { dg-warning "'fac2' reading 4 bytes from a region of size 2 " }
  fac2 (&i);                  // { dg-warning "'fac2' reading 4 bytes from a region of size 2 " }

  fxac2 (0);                  // { dg-warning "\\\[-Wnonnull" }
  fxac2 (a2);
  fxac2 (a1);                 // { dg-warning "'fxac2' reading 4 bytes from a region of size 2 " }
  fxac2 (&i);                 // { dg-warning "'fxac2' reading 4 bytes from a region of size 2 " }

  facs2 (0);                  // { dg-warning "\\\[-Wnonnull" }
  facs2 (a2);
  facs2 (a1);                 // { dg-warning "'facs2' reading 4 bytes from a region of size 2 " }
  facs2 (&i);                 // { dg-warning "'facs2' reading 4 bytes from a region of size 2 " }

  fvlac (1, 0);
  fvlacS (1, 0);              // { dg-warning "\\\[-Wnonnull" }
  fvlac (1, &i);
  fvlac (2, a2);
  fvlac (2, a1);              // { dg-warning "'fvlac' reading 4 bytes from a region of size 2 " }
  fvlac (2, &i);              // { dg-warning "'fvlac' reading 4 bytes from a region of size 2 " }
}


void fca3x5 (int16_t[3][5]);
void fcas5x7 (int16_t[static 5][7]);

struct Snx5 { int16_t a3x5[3][5], a2x5[2][5], a1x5[1][5]; };
struct Snx7 { int16_t a5x7[5][7], a4x7[4][7], a1x7[1][7]; };
struct S0x7 { int x; int16_t a0x7[0][7]; };

void test_array_2_dim (struct Snx5 *px5, struct Snx7 *px7, struct S0x7 *p0x7)
{
  int16_t a0x5[0][5], a1x5[1][5], a2x5[2][5], a3x5[3][5], a4x5[4][5];

  fca3x5 (a3x5);
  fca3x5 (a4x5);
  fca3x5 (a2x5);              // { dg-warning "'fca3x5' accessing 30 bytes in a region of size 20" }
  fca3x5 (a1x5);              // { dg-warning "'fca3x5' accessing 30 bytes in a region of size 10" }
  fca3x5 (a0x5);              // { dg-warning "'fca3x5' accessing 30 bytes in a region of size 0" }

  fca3x5 (px5->a3x5);
  fca3x5 (px5->a2x5);         // { dg-warning "'fca3x5' accessing 30 bytes in a region of size 20" }
  fca3x5 (px5->a1x5);         // { dg-warning "'fca3x5' accessing 30 bytes in a region of size 10" "pr96346" { xfail *-*-* } }

  {
    int16_t (*pa2x5)[5] = &a2x5[0];
    fca3x5 (pa2x5);           // { dg-warning "'fca3x5' accessing 30 bytes in a region of size 10" }
    ++pa2x5;
    fca3x5 (pa2x5);           // { dg-warning "'fca3x5' accessing 30 bytes " }
  }

  int16_t a0x7[0][7], a1x7[1][7], a4x7[4][7], a5x7[5][7], a99x7[99][7];
  fcas5x7 (a99x7);
  fcas5x7 (a5x7);
  fcas5x7 (a4x7);             // { dg-warning "'fcas5x7' accessing 70 bytes in a region of size 56" }
  fcas5x7 (a1x7);             // { dg-warning "'fcas5x7' accessing 70 bytes in a region of size 14" }
  fcas5x7 (a0x7);             // { dg-warning "'fcas5x7' accessing 70 bytes in a region of size 0" }

  fcas5x7 (px7->a5x7);
  fcas5x7 (px7->a4x7);        // { dg-warning "'fcas5x7' accessing 70 bytes in a region of size 56" }
  fcas5x7 (px7->a1x7);        // { dg-warning "'fcas5x7' accessing 70 bytes in a region of size 14" "pr96346" { xfail *-*-* } }

  fcas5x7 (p0x7->a0x7);       // { dg-warning "'fcas5x7' accessing 70 bytes in a region of size 0" "pr96346" { xfail *-*-* } }
}
