/* PR middle-end/84051 - missing -Warray-bounds on an out-of-bounds access
   via an array pointer
   { dg-do compile }
   { dg-options "-O2 -Wall -ftrack-macro-expansion=0" } */

void sink (void*, ...);
#define T(x) sink (0, x)

void
test_note (int (*pia3)[3])    // { dg-message "while referencing 'pia3'" }
{
  int i = 0;
  T ((*pia3)[i++]);
  T ((*pia3)[i++]);
  T ((*pia3)[i++]);
  T ((*pia3)[i++]);           // { dg-warning "array subscript 3 is (above|outside) array bounds of 'int\\\[3]'" }
  T ((*pia3)[i++]);           // { dg-warning "array subscript 4 is (above|outside) array bounds of 'int\\\[3]'" }

  {
    /* Regrettably, the following isn't diagnosed because it's represented
       the same as the possibly valid access below:
         MEM[(int *)a_1(D) + 36B] = 0;  */
    int *p0 = pia3[0];
    T (p0[3]);                // { dg-warning "array subscript 3 is (above|outside) array bounds of 'int\\\[3]'" "pr?????" { xfail *-*-* } }

    int *p1 = pia3[3];
    T (p1[0]);                // okay
  }
}

void test_a1_cst (_Bool (*pba0)[0], char (*pca1)[1],
		  short (*psa2)[2], int (*pia3)[3])
{
  T ((*pba0)[-1]);            // { dg-warning "array subscript -1 is (above|outside) array bounds of '_Bool\\\[0]'" }
  T ((*pba0)[0]);             // { dg-warning "array subscript 0 is (above|outside) array bounds of '_Bool\\\[0]'" }
  T ((*pba0)[1]);             // { dg-warning "array subscript 1 is (above|outside) array bounds of '_Bool\\\[0]'" }
  T ((*pba0)[2]);             // { dg-warning "array subscript 2 is (above|outside) array bounds of '_Bool\\\[0]'" }
  T ((*pba0)[12]);            // { dg-warning "array subscript 12 is (above|outside) array bounds of '_Bool\\\[0]'" }

  T ((*pca1)[-1]);            // { dg-warning "array subscript -1 is (below|outside) array bounds of 'char\\\[1]'" }
  T ((*pca1)[0]);
  T ((*pca1)[1]);             // { dg-warning "array subscript 1 is (above|outside) array bounds of 'char\\\[1]'" }
  T ((*pca1)[2]);             // { dg-warning "array subscript 2 is (above|outside) array bounds of 'char\\\[1]'" }
  T ((*pca1)[123]);           // { dg-warning "array subscript 123 is (above|outside) array bounds of 'char\\\[1]'" }

  T ((*psa2)[-1]);            // { dg-warning "array subscript -1 is (below|outside) array bounds of 'short int\\\[2]'" }
  T ((*psa2)[0]);
  T ((*psa2)[1]);
  T ((*psa2)[2]);             // { dg-warning "array subscript 2 is (above|outside) array bounds of 'short int\\\[2]'" }
  T ((*psa2)[1234]);          // { dg-warning "array subscript 1234 is (above|outside) array bounds of 'short int\\\[2]'" }

  T ((*pia3)[-1]);            // { dg-warning "array subscript -1 is (below|outside) array bounds of 'int\\\[3]'" }
  T ((*pia3)[0]);
  T ((*pia3)[1]);
  T ((*pia3)[2]);
  T ((*pia3)[3]);             // { dg-warning "array subscript 3 is (above|outside) array bounds of 'int\\\[3]'" }
  T ((*pia3)[12345]);         // { dg-warning "array subscript 12345 is (above|outside) array bounds of 'int\\\[3]'" }
}


void test_a2_cst (_Bool (*pba0_1)[0][1], char (*pca1_2)[1][2],
		  short (*psa2_3)[2][3], int (*pia3_4)[3][4])
{
  T ((*pba0_1)[-1][-1]);        // { dg-warning "array subscript -1 is (below|outside) array bounds of '_Bool\\\[1]'" }
  T ((*pba0_1)[-1][0]);         // { dg-warning "array subscript -1 is (above|outside) array bounds of '_Bool\\\[0]\\\[1]'" }

  T ((*pba0_1)[0][-1]);         // { dg-warning "array subscript -1 is (below|outside) array bounds of '_Bool\\\[1]'" }
  T ((*pba0_1)[0][0]);          // { dg-warning "array subscript 0 is (above|outside) array bounds of '_Bool\\\[0]\\\[1]'" }
  T ((*pba0_1)[0][1]);          // { dg-warning "array subscript 1 is (above|outside) array bounds of '_Bool\\\[1]'" }
  T ((*pba0_1)[0][2]);          // { dg-warning "array subscript 2 is (above|outside) array bounds of '_Bool\\\[1]'" }
  T ((*pba0_1)[0][12]);         // { dg-warning "array subscript 12 is (above|outside) array bounds of '_Bool\\\[1]'" }

  T ((*pba0_1)[1][-1]);         // { dg-warning "array subscript -1 is (below|outside) array bounds of '_Bool\\\[1]'" }
  T ((*pba0_1)[1][0]);          // { dg-warning "array subscript 1 is (above|outside) array bounds of '_Bool\\\[0]\\\[1]'" }
  T ((*pba0_1)[1][1]);          // { dg-warning "array subscript 1 is (above|outside) array bounds of '_Bool\\\[1]'" }
  T ((*pba0_1)[1][2]);          // { dg-warning "array subscript 2 is (above|outside) array bounds of '_Bool\\\[1]'" }
  T ((*pba0_1)[1][12]);         // { dg-warning "array subscript 12 is (above|outside) array bounds of '_Bool\\\[1]'" }


  T ((*pca1_2)[0][0]);
  T ((*pca1_2)[0][1]);
  T ((*pca1_2)[0][2]);          // { dg-warning "array subscript 2 is (above|outside) array bounds of 'char\\\[2]'" }

  T ((*pca1_2)[1][0]);          // { dg-warning "array subscript 1 is (above|outside) array bounds of 'char\\\[1]\\\[2]'" }
  T ((*pca1_2)[1][1]);          // { dg-warning "array subscript 1 is (above|outside) array bounds of 'char\\\[1]\\\[2]'" }
  T ((*pca1_2)[1][2]);          // { dg-warning "array subscript 2 is (above|outside) array bounds of 'char\\\[2]'" }


  T ((*psa2_3)[0][0]);
  T ((*psa2_3)[0][1]);
  T ((*psa2_3)[0][2]);
  T ((*psa2_3)[0][3]);          // { dg-warning "array subscript 3 is (above|outside) array bounds of 'short int\\\[3]'" }

  T ((*psa2_3)[1][0]);
  T ((*psa2_3)[1][1]);
  T ((*psa2_3)[1][2]);
  T ((*psa2_3)[1][3]);          // { dg-warning "array subscript 3 is (above|outside) array bounds of 'short int\\\[3]'" }

  T ((*psa2_3)[2][0]);          // { dg-warning "array subscript 2 is (above|outside) array bounds of 'short int\\\[2]\\\[3]'" }
  T ((*psa2_3)[2][1]);          // { dg-warning "array subscript 2 is (above|outside) array bounds of 'short int\\\[2]\\\[3]'" }
  T ((*psa2_3)[2][2]);          // { dg-warning "array subscript 2 is (above|outside) array bounds of 'short int\\\[2]\\\[3]'" }
  T ((*psa2_3)[2][3]);          // { dg-warning "array subscript 3 is (above|outside) array bounds of 'short int\\\[3]'" }


  T ((*pia3_4)[0][0]);
  T ((*pia3_4)[0][1]);
  T ((*pia3_4)[0][2]);
  T ((*pia3_4)[0][3]);
  T ((*pia3_4)[0][4]);          // { dg-warning "array subscript 4 is (above|outside) array bounds of 'int\\\[4]'" }

  T ((*pia3_4)[1][0]);
  T ((*pia3_4)[1][1]);
  T ((*pia3_4)[1][2]);
  T ((*pia3_4)[1][3]);
  T ((*pia3_4)[1][4]);          // { dg-warning "array subscript 4 is (above|outside) array bounds of 'int\\\[4]'" }

  T ((*pia3_4)[2][0]);
  T ((*pia3_4)[2][1]);
  T ((*pia3_4)[2][2]);
  T ((*pia3_4)[2][3]);
  T ((*pia3_4)[2][4]);          // { dg-warning "array subscript 4 is (above|outside) array bounds of 'int\\\[4]'" }

  T ((*pia3_4)[3][0]);          // { dg-warning "array subscript 3 is (above|outside) array bounds of 'int\\\[3]\\\[4]'" }
  T ((*pia3_4)[3][1]);          // { dg-warning "array subscript 3 is (above|outside) array bounds of 'int\\\[3]\\\[4]'" }
  T ((*pia3_4)[3][2]);          // { dg-warning "array subscript 3 is (above|outside) array bounds of 'int\\\[3]\\\[4]'" }
  T ((*pia3_4)[3][3]);          // { dg-warning "array subscript 3 is (above|outside) array bounds of 'int\\\[3]\\\[4]'" }
  T ((*pia3_4)[3][4]);          // { dg-warning "array subscript 4 is (above|outside) array bounds of 'int\\\[4]'" }
}


typedef int IA4[4];
typedef IA4 IA3_4[3];

void test_a2_var (IA3_4 *pia3_4)
{
  {
    IA4 *pia4 = &(*pia3_4)[0];

    T ((*pia4)[-1]);            // { dg-warning "array subscript -1 is (below|outside) array bounds of 'IA4'" }
    T ((*pia4)[0]);
    T ((*pia4)[1]);
    T ((*pia4)[2]);
    T ((*pia4)[3]);
    T ((*pia4)[4]);             // { dg-warning "array subscript 4 is (above|outside) array bounds of 'IA4'" }
  }

  {
    IA4 *pia4 = &(*pia3_4)[1];

    T ((*pia4)[-1]);            // { dg-warning "array subscript -1 is (below|outside) array bounds of 'IA4'" }
    T ((*pia4)[0]);
    T ((*pia4)[1]);
    T ((*pia4)[2]);
    T ((*pia4)[3]);
    T ((*pia4)[4]);             // { dg-warning "array subscript 4 is (above|outside) array bounds of 'IA4'" }
  }

  {
    IA4 *pia4 = &(*pia3_4)[2];

    T ((*pia4)[-1]);            // { dg-warning "array subscript -1 is (below|outside) array bounds of 'IA4'" }
    T ((*pia4)[0]);
    T ((*pia4)[1]);
    T ((*pia4)[2]);
    T ((*pia4)[3]);
    T ((*pia4)[4]);             // { dg-warning "array subscript 4 is (above|outside) array bounds of 'IA4'" }
  }

  {
    IA4 *pia4 = &(*pia3_4)[3];

    T ((*pia4)[-1]);            // { dg-warning "\\\[-Warray-bounds" }
    /* The following aren't diagnosed unless N itself is out of bounds
       because thanks to the MEM_REF they're indistinguishable from
       possibly valid accesses:
         MEM[(int[4] *)pia3_4_2(D) + 48B][N];  */
    T ((*pia4)[0]);             // { dg-warning "\\\[-Warray-bounds" "pr?????" { xfail *-*-* } }
    T ((*pia4)[1]);             // { dg-warning "\\\[-Warray-bounds" "pr?????" { xfail *-*-* } }
    T ((*pia4)[2]);             // { dg-warning "\\\[-Warray-bounds" "pr?????" { xfail *-*-* } }
    T ((*pia4)[3]);             // { dg-warning "\\\[-Warray-bounds" "pr?????" { xfail *-*-* } }
    T ((*pia4)[4]);             // { dg-warning "\\\[-Warray-bounds" }
  }
}


struct S { IA3_4 *pia3_4; };
typedef struct S S5[5];
typedef S5 S5_7[7];

void test_s5_7 (S5_7 *ps5_7)
{
  {
    S5 *ps5 = &(*ps5_7)[0];
    T ((*ps5)[0]);
    T ((*(*ps5)[0].pia3_4)[0][0]);
    T ((*(*ps5)[0].pia3_4)[2][3]);
    T ((*(*ps5)[0].pia3_4)[2][4]);    // { dg-warning "array subscript 4 is above array bounds of 'IA4'" }

    T ((*(*ps5)[1].pia3_4)[2][3]);
    T ((*(*ps5)[5].pia3_4)[2][3]);    // { dg-warning "array subscript 5 is above array bounds of 'S5'" }
  }
}
