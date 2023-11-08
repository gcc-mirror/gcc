/* Test errors for bad string literal concatenation.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

void *pLU = L"" U""; /* { dg-error "non-standard concatenation" } */
void *pL_U = L"" "" U""; /* { dg-error "non-standard concatenation" } */
void *pLu = L"" u""; /* { dg-error "non-standard concatenation" } */
void *pL_u = L"" "" u""; /* { dg-error "non-standard concatenation" } */
void *pLu8 = L"" u8""; /* { dg-error "non-standard concatenation" } */
void *pL_u8 = L"" "" u8""; /* { dg-error "non-standard concatenation" } */

void *pUL = U"" L""; /* { dg-error "non-standard concatenation" } */
void *pU_L = U"" "" L""; /* { dg-error "non-standard concatenation" } */
void *pUu = U"" u""; /* { dg-error "non-standard concatenation" } */
void *pU_u = U"" "" u""; /* { dg-error "non-standard concatenation" } */
void *pUu8 = U"" u8""; /* { dg-error "non-standard concatenation" } */
void *pU_u8 = U"" "" u8""; /* { dg-error "non-standard concatenation" } */

void *puL = u"" L""; /* { dg-error "non-standard concatenation" } */
void *pu_L = u"" "" L""; /* { dg-error "non-standard concatenation" } */
void *puU = u"" U""; /* { dg-error "non-standard concatenation" } */
void *pu_U = u"" "" U""; /* { dg-error "non-standard concatenation" } */
void *puu8 = u"" u8""; /* { dg-error "non-standard concatenation" } */
void *pu_u8 = u"" "" u8""; /* { dg-error "non-standard concatenation" } */

void *pu8L = u8"" L""; /* { dg-error "non-standard concatenation" } */
void *pu8_L = u8"" "" L""; /* { dg-error "non-standard concatenation" } */
void *pu8U = u8"" U""; /* { dg-error "non-standard concatenation" } */
void *pu8_U = u8"" "" U""; /* { dg-error "non-standard concatenation" } */
void *pu8u = u8"" u""; /* { dg-error "non-standard concatenation" } */
void *pu8_u = u8"" "" u""; /* { dg-error "non-standard concatenation" } */
