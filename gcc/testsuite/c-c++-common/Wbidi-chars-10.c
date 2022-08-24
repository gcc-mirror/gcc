/* PR preprocessor/103026 */
/* { dg-do compile } */
/* { dg-options "-Wbidi-chars=unpaired,ucn" } */
/* More nesting testing.  */

/* RLE‫ LRI⁦ PDF‬ PDI⁩*/
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int LRE_\u202a_PDF_\u202c;
int LRE_\u202a_PDF_\u202c_LRE_\u202a_PDF_\u202c;
int LRE_\u202a_LRI_\u2066_PDF_\u202c_PDI_\u2069;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int RLE_\u202b_RLI_\u2067_PDF_\u202c_PDI_\u2069;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int RLE_\u202b_RLI_\u2067_PDI_\u2069_PDF_\u202c;
int FSI_\u2068_LRO_\u202d_PDI_\u2069_PDF_\u202c;
int FSI_\u2068;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int FSI_\u2068_PDI_\u2069;
int FSI_\u2068_FSI_\u2068_PDI_\u2069;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int RLI_\u2067_RLI_\u2067_RLI_\u2067_RLI_\u2067_RLI_\u2067_RLI_\u2067_RLI_\u2067_PDI_\u2069_PDI_\u2069_PDI_\u2069_PDI_\u2069_PDI_\u2069_PDI_\u2069_PDI_\u2069;
int RLI_\u2067_RLI_\u2067_RLI_\u2067_RLI_\u2067_RLI_\u2067_RLI_\u2067_RLI_\u2067_PDI_\u2069_PDI_\u2069_PDI_\u2069_PDI_\u2069_PDI_\u2069_PDI_\u2069;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int RLI_\u2067_RLI_\u2067_RLI_\u2067_RLI_\u2067_RLI_\u2067_RLI_\u2067_RLI_\u2067_PDI_\u2069_PDI_\u2069_PDI_\u2069_PDI_\u2069_PDI_\u2069_PDI_\u2069_PDF_\u202c;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int RLI_\u2067_RLI_\u2067_RLI_\u2067_RLI_\u2067_FSI_\u2068_PDI_\u2069_PDI_\u2069_PDI_\u2069_PDI_\u2069;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
