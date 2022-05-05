/* PR preprocessor/103026 */
/* { dg-do compile } */
/* { dg-options "-Wbidi-chars=unpaired,ucn" } */
/* Test PDI handling, which also pops any subsequent LREs, RLEs, LROs,
   or RLOs.  */

/* LRI_⁦_LRI_⁦_RLE_‫_RLE_‫_RLE_‫_PDI_⁩*/
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
// LRI_⁦_RLE_‫_RLE_‫_RLE_‫_PDI_⁩
// LRI_⁦_RLO_‮_RLE_‫_RLE_‫_PDI_⁩
// LRI_⁦_RLO_‮_RLE_‫_PDI_⁩
// FSI_⁨_RLO_‮_PDI_⁩
// FSI_⁨_FSI_⁨_RLO_‮_PDI_⁩
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */

int LRI_\u2066_LRI_\u2066_LRE_\u202a_LRE_\u202a_LRE_\u202a_PDI_\u2069;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int LRI_\u2066_LRI_\u2066_LRE_\u202a_LRE_\u202a_LRE_\u202a_PDI_\u2069_PDI_\u2069;
int LRI_\u2066_LRI_\u2066_LRI_\u2066_LRE_\u202a_LRE_\u202a_LRE_\u202a_PDI_\u2069_PDI_\u2069;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int PDI_\u2069;
int LRI_\u2066_PDI_\u2069;
int RLI_\u2067_PDI_\u2069;
int LRE_\u202a_LRI_\u2066_PDI_\u2069;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int LRI_\u2066_LRE_\u202a_PDF_\u202c_PDI_\u2069;
int LRI_\u2066_LRE_\u202a_LRE_\u202a_PDF_\u202c_PDI_\u2069;
int RLI_\u2067_LRI_\u2066_LRE_\u202a_LRE_\u202a_PDF_\u202c_PDI_\u2069;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int FSI_\u2068_LRI_\u2066_LRE_\u202a_LRE_\u202a_PDF_\u202c_PDI_\u2069;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int RLO_\u202e_PDI_\u2069;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int RLI_\u2067_PDI_\u2069_RLI_\u2067;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int FSI_\u2068_PDF_\u202c_PDI_\u2069;
int FSI_\u2068_FSI_\u2068_PDF_\u202c_PDI_\u2069;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
