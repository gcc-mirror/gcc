/* PR preprocessor/103026 */
/* { dg-do compile } */
/* { dg-options "-Wbidi-chars=ucn,unpaired" } */
/* Test nesting of bidi chars in various contexts.  */

/* Terminated by the wrong char:  */
/* a b c LRE‪ 1 2 3 PDI⁩ x y z */
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
/* a b c RLE‫ 1 2 3 PDI⁩ x y  z*/
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
/* a b c LRO‭ 1 2 3 PDI⁩ x y z */
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
/* a b c RLO‮ 1 2 3 PDI⁩ x y z */
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
/* a b c LRI⁦ 1 2 3 PDF‬ x y z */
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
/* a b c RLI⁧ 1 2 3 PDF‬ x y z */
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
/* a b c FSI⁨ 1 2 3 PDF‬ x y  z*/
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */

/* LRE‪ PDF‬ */
/* LRE‪ LRE‪ PDF‬ PDF‬ */
/* PDF‬ LRE‪ PDF‬ */
/* LRE‪ PDF‬ LRE‪ PDF‬ */
/* LRE‪ LRE‪ PDF‬ */
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
/* PDF‬ LRE‪ */
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */

// a b c LRE‪ 1 2 3 PDI⁩ x y z
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
// a b c RLE‫ 1 2 3 PDI⁩ x y  z*/
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
// a b c LRO‭ 1 2 3 PDI⁩ x y z 
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
// a b c RLO‮ 1 2 3 PDI⁩ x y z 
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
// a b c LRI⁦ 1 2 3 PDF‬ x y z 
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
// a b c RLI⁧ 1 2 3 PDF‬ x y z 
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
// a b c FSI⁨ 1 2 3 PDF‬ x y  z
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */

// LRE‪ PDF‬ 
// LRE‪ LRE‪ PDF‬ PDF‬
// PDF‬ LRE‪ PDF‬
// LRE‪ PDF‬ LRE‪ PDF‬
// LRE‪ LRE‪ PDF‬
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
// PDF‬ LRE‪
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */

void
g1 ()
{
  const char *s1 = "a b c LRE‪ 1 2 3 PDI⁩ x y z";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s2 = "a b c LRE\u202a 1 2 3 PDI\u2069 x y z";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s3 = "a b c RLE‫ 1 2 3 PDI⁩ x y ";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s4 = "a b c RLE\u202b 1 2 3 PDI\u2069 x y z";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s5 = "a b c LRO‭ 1 2 3 PDI⁩ x y z";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s6 = "a b c LRO\u202d 1 2 3 PDI\u2069 x y z";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s7 = "a b c RLO‮ 1 2 3 PDI⁩ x y z";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s8 = "a b c RLO\u202e 1 2 3 PDI\u2069 x y z";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s9 = "a b c LRI⁦ 1 2 3 PDF‬ x y z";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s10 = "a b c LRI\u2066 1 2 3 PDF\u202c x y z";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s11 = "a b c RLI⁧ 1 2 3 PDF‬ x y z\
    ";
/* { dg-warning "unpaired" "" { target *-*-* } .-2 } */
  const char *s12 = "a b c RLI\u2067 1 2 3 PDF\u202c x y z";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s13 = "a b c FSI⁨ 1 2 3 PDF‬ x y z";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s14 = "a b c FSI\u2068 1 2 3 PDF\u202c x y z";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s15 = "PDF‬ LRE‪";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s16 = "PDF\u202c LRE\u202a";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s17 = "LRE‪ PDF‬";
  const char *s18 = "LRE\u202a PDF\u202c";
  const char *s19 = "LRE‪ LRE‪ PDF‬ PDF‬";
  const char *s20 = "LRE\u202a LRE\u202a PDF\u202c PDF\u202c";
  const char *s21 = "PDF‬ LRE‪ PDF‬";
  const char *s22 = "PDF\u202c LRE\u202a PDF\u202c";
  const char *s23 = "LRE‪ LRE‪ PDF‬";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s24 = "LRE\u202a LRE\u202a PDF\u202c";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s25 = "PDF‬ LRE‪";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s26 = "PDF\u202c LRE\u202a";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s27 = "PDF‬ LRE\u202a";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s28 = "PDF\u202c LRE‪";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
}

int aLRE‪bPDI⁩;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int A\u202aB\u2069C;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int aRLE‫bPDI⁩;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int a\u202bB\u2069c;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int aLRO‭bPDI⁩;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int a\u202db\u2069c2;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int aRLO‮bPDI⁩;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int a\u202eb\u2069;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int aLRI⁦bPDF‬;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int a\u2066b\u202c;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int aRLI⁧bPDF‬c
;
/* { dg-warning "unpaired" "" { target *-*-* } .-2 } */
int a\u2067b\u202c;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int aFSI⁨bPDF‬;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int a\u2068b\u202c;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int aFSI⁨bPD\u202C;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int aFSI\u2068bPDF‬_;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int aLRE‪bPDF‬b; 
int A\u202aB\u202c;
int a_LRE‪_LRE‪_b_PDF‬_PDF‬;
int A\u202aA\u202aB\u202cB\u202c;
int aPDF‬bLREadPDF‬;
int a_\u202C_\u202a_\u202c;
int a_LRE‪_b_PDF‬_c_LRE‪_PDF‬;
int a_\u202a_\u202c_\u202a_\u202c_;
int a_LRE‪_b_PDF‬_c_LRE‪;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int a_\u202a_\u202c_\u202a_;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
