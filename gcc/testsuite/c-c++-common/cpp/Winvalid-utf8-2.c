// P2295R6 - Support for UTF-8 as a portable source file encoding
// This test intentionally contains various byte sequences which are not valid UTF-8
// { dg-do preprocess { target { c || c++11 } } }
// { dg-require-effective-target wchar }
// { dg-options "-finput-charset=UTF-8 -Winvalid-utf8" }
// { dg-additional-options "-std=gnu99" { target c } }

#ifndef __cplusplus
#include <wchar.h>
typedef __CHAR16_TYPE__ char16_t;
typedef __CHAR32_TYPE__ char32_t;
#endif

char32_t a = U'�';				// { dg-warning "invalid UTF-8 character <80>" }
char32_t b = U'�';				// { dg-warning "invalid UTF-8 character <bf>" }
char32_t c = U'�';				// { dg-warning "invalid UTF-8 character <c0>" }
char32_t d = U'�';				// { dg-warning "invalid UTF-8 character <c1>" }
char32_t e = U'�';				// { dg-warning "invalid UTF-8 character <f5>" }
char32_t f = U'�';				// { dg-warning "invalid UTF-8 character <ff>" }
char32_t g = U'�';				// { dg-warning "invalid UTF-8 character <c2>" }
char32_t h = U'�';				// { dg-warning "invalid UTF-8 character <e0>" }
char32_t i = U'���';				// { dg-warning "invalid UTF-8 character <e0><80><bf>" }
char32_t j = U'���';				// { dg-warning "invalid UTF-8 character <e0><9f><80>" }
char32_t k = U'�';				// { dg-warning "invalid UTF-8 character <e0><bf>" }
char32_t l = U'�';				// { dg-warning "invalid UTF-8 character <ec><80>" }
char32_t m = U'�';				// { dg-warning "invalid UTF-8 character <ed><a0><80>" }
char32_t n = U'����';				// { dg-warning "invalid UTF-8 character <f0><80><80><80>" }
char32_t o = U'����';				// { dg-warning "invalid UTF-8 character <f0><8f><bf><bf>" }
char32_t p = U'����';				// { dg-warning "invalid UTF-8 character <f4><90><80><80>" }
char32_t q = U'������';				// { dg-warning "invalid UTF-8 character <fd><bf><bf><bf>" }
						// { dg-warning "invalid UTF-8 character <bf>" "" { target *-*-* } .-1 }
const char32_t *A = U"߿ࠀ퟿𐀀􏿿";	// { dg-bogus "invalid UTF-8 character" }
const char32_t *B = U"�";			// { dg-warning "invalid UTF-8 character <80>" }
const char32_t *C = U"�";			// { dg-warning "invalid UTF-8 character <bf>" }
const char32_t *D = U"�";			// { dg-warning "invalid UTF-8 character <c0>" }
const char32_t *E = U"�";			// { dg-warning "invalid UTF-8 character <c1>" }
const char32_t *F = U"�";			// { dg-warning "invalid UTF-8 character <f5>" }
const char32_t *G = U"�";			// { dg-warning "invalid UTF-8 character <ff>" }
const char32_t *H = U"�";			// { dg-warning "invalid UTF-8 character <c2>" }
const char32_t *I = U"�";			// { dg-warning "invalid UTF-8 character <e0>" }
const char32_t *J = U"���";			// { dg-warning "invalid UTF-8 character <e0><80><bf>" }
const char32_t *K = U"���";			// { dg-warning "invalid UTF-8 character <e0><9f><80>" }
const char32_t *L = U"�";			// { dg-warning "invalid UTF-8 character <e0><bf>" }
const char32_t *M = U"�";			// { dg-warning "invalid UTF-8 character <ec><80>" }
const char32_t *N = U"�";			// { dg-warning "invalid UTF-8 character <ed><a0><80>" }
const char32_t *O = U"����";			// { dg-warning "invalid UTF-8 character <f0><80><80><80>" }
const char32_t *P = U"����";			// { dg-warning "invalid UTF-8 character <f0><8f><bf><bf>" }
const char32_t *Q = U"����";			// { dg-warning "invalid UTF-8 character <f4><90><80><80>" }
const char32_t *R = U"������";			// { dg-warning "invalid UTF-8 character <fd><bf><bf><bf>" }
						// { dg-warning "invalid UTF-8 character <bf>" "" { target *-*-* } .-1 }
const char32_t *A1 = UR"(߿ࠀ퟿𐀀􏿿)"; // { dg-bogus "invalid UTF-8 character" }
const char32_t *B1 = UR"(�)";			// { dg-warning "invalid UTF-8 character <80>" }
const char32_t *C1 = UR"(�)";			// { dg-warning "invalid UTF-8 character <bf>" }
const char32_t *D1 = UR"(�)";			// { dg-warning "invalid UTF-8 character <c0>" }
const char32_t *E1 = UR"(�)";			// { dg-warning "invalid UTF-8 character <c1>" }
const char32_t *F1 = UR"(�)";			// { dg-warning "invalid UTF-8 character <f5>" }
const char32_t *G1 = UR"(�)";			// { dg-warning "invalid UTF-8 character <ff>" }
const char32_t *H1 = UR"(�)";			// { dg-warning "invalid UTF-8 character <c2>" }
const char32_t *I1 = UR"(�)";			// { dg-warning "invalid UTF-8 character <e0>" }
const char32_t *J1 = UR"(���)";			// { dg-warning "invalid UTF-8 character <e0><80><bf>" }
const char32_t *K1 = UR"(���)";			// { dg-warning "invalid UTF-8 character <e0><9f><80>" }
const char32_t *L1 = UR"(�)";			// { dg-warning "invalid UTF-8 character <e0><bf>" }
const char32_t *M1 = UR"(�)";			// { dg-warning "invalid UTF-8 character <ec><80>" }
const char32_t *N1 = UR"(�)";			// { dg-warning "invalid UTF-8 character <ed><a0><80>" }
const char32_t *O1 = UR"(����)";		// { dg-warning "invalid UTF-8 character <f0><80><80><80>" }
const char32_t *P1 = UR"(����)";		// { dg-warning "invalid UTF-8 character <f0><8f><bf><bf>" }
const char32_t *Q1 = UR"(����)";		// { dg-warning "invalid UTF-8 character <f4><90><80><80>" }
const char32_t *R1 = UR"(������)";		// { dg-warning "invalid UTF-8 character <fd><bf><bf><bf>" }
						// { dg-warning "invalid UTF-8 character <bf>" "" { target *-*-* } .-1 }
const char *A2 = u8"߿ࠀ퟿𐀀􏿿";	// { dg-bogus "invalid UTF-8 character" }
const char *B2 = u8"�";				// { dg-warning "invalid UTF-8 character <80>" }
const char *C2 = u8"�";				// { dg-warning "invalid UTF-8 character <bf>" }
const char *D2 = u8"�";				// { dg-warning "invalid UTF-8 character <c0>" }
const char *E2 = u8"�";				// { dg-warning "invalid UTF-8 character <c1>" }
const char *F2 = u8"�";				// { dg-warning "invalid UTF-8 character <f5>" }
const char *G2 = u8"�";				// { dg-warning "invalid UTF-8 character <ff>" }
const char *H2 = u8"�";				// { dg-warning "invalid UTF-8 character <c2>" }
const char *I2 = u8"�";				// { dg-warning "invalid UTF-8 character <e0>" }
const char *J2 = u8"���";			// { dg-warning "invalid UTF-8 character <e0><80><bf>" }
const char *K2 = u8"���";			// { dg-warning "invalid UTF-8 character <e0><9f><80>" }
const char *L2 = u8"�";			// { dg-warning "invalid UTF-8 character <e0><bf>" }
const char *M2 = u8"�";			// { dg-warning "invalid UTF-8 character <ec><80>" }
const char *N2 = u8"�";			// { dg-warning "invalid UTF-8 character <ed><a0><80>" }
const char *O2 = u8"����";			// { dg-warning "invalid UTF-8 character <f0><80><80><80>" }
const char *P2 = u8"����";			// { dg-warning "invalid UTF-8 character <f0><8f><bf><bf>" }
const char *Q2 = u8"����";			// { dg-warning "invalid UTF-8 character <f4><90><80><80>" }
const char *R2 = u8"������";			// { dg-warning "invalid UTF-8 character <fd><bf><bf><bf>" }
						// { dg-warning "invalid UTF-8 character <bf>" "" { target *-*-* } .-1 }
