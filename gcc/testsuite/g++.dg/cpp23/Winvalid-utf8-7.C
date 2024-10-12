// P2295R6 - Support for UTF-8 as a portable source file encoding
// This test intentionally contains various byte sequences which are not valid UTF-8
// { dg-do preprocess { target c++11 } }
// { dg-options "-finput-charset=UTF-8 -pedantic-errors" }

char32_t a = U'€';				// { dg-error "invalid UTF-8 character '<80>'" "" { target c++23 } }
char32_t b = U'¿';				// { dg-error "invalid UTF-8 character '<bf>'" "" { target c++23 } }
char32_t c = U'À';				// { dg-error "invalid UTF-8 character '<c0>'" "" { target c++23 } }
char32_t d = U'Á';				// { dg-error "invalid UTF-8 character '<c1>'" "" { target c++23 } }
char32_t e = U'õ';				// { dg-error "invalid UTF-8 character '<f5>'" "" { target c++23 } }
char32_t f = U'ÿ';				// { dg-error "invalid UTF-8 character '<ff>'" "" { target c++23 } }
char32_t g = U'Â';				// { dg-error "invalid UTF-8 character '<c2>'" "" { target c++23 } }
char32_t h = U'à';				// { dg-error "invalid UTF-8 character '<e0>'" "" { target c++23 } }
char32_t i = U'à€¿';				// { dg-error "invalid UTF-8 character '<e0><80><bf>'" "" { target c++23 } }
char32_t j = U'àŸ€';				// { dg-error "invalid UTF-8 character '<e0><9f><80>'" "" { target c++23 } }
char32_t k = U'à¿';				// { dg-error "invalid UTF-8 character '<e0><bf>'" "" { target c++23 } }
char32_t l = U'ì€';				// { dg-error "invalid UTF-8 character '<ec><80>'" "" { target c++23 } }
char32_t m = U'í €';				// { dg-error "invalid UTF-8 character '<ed><a0><80>'" "" { target c++23 } }
char32_t n = U'ğ€€€';				// { dg-error "invalid UTF-8 character '<f0><80><80><80>'" "" { target c++23 } }
char32_t o = U'ğ¿¿';				// { dg-error "invalid UTF-8 character '<f0><8f><bf><bf>'" "" { target c++23 } }
char32_t p = U'ô€€';				// { dg-error "invalid UTF-8 character '<f4><90><80><80>'" "" { target c++23 } }
char32_t q = U'ı¿¿¿¿¿';				// { dg-error "invalid UTF-8 character '<fd><bf><bf><bf>'" "" { target c++23 } }
						// { dg-error "invalid UTF-8 character '<bf>'" "" { target c++23 } .-1 }
auto A = U"Â€ß¿à €íŸ¿î€€ğ€€ô¿¿";		// { dg-bogus "invalid UTF-8 character" }
auto B = U"€";					// { dg-error "invalid UTF-8 character '<80>'" "" { target c++23 } }
auto C = U"¿";					// { dg-error "invalid UTF-8 character '<bf>'" "" { target c++23 } }
auto D = U"À";					// { dg-error "invalid UTF-8 character '<c0>'" "" { target c++23 } }
auto E = U"Á";					// { dg-error "invalid UTF-8 character '<c1>'" "" { target c++23 } }
auto F = U"õ";					// { dg-error "invalid UTF-8 character '<f5>'" "" { target c++23 } }
auto G = U"ÿ";					// { dg-error "invalid UTF-8 character '<ff>'" "" { target c++23 } }
auto H = U"Â";					// { dg-error "invalid UTF-8 character '<c2>'" "" { target c++23 } }
auto I = U"à";					// { dg-error "invalid UTF-8 character '<e0>'" "" { target c++23 } }
auto J = U"à€¿";				// { dg-error "invalid UTF-8 character '<e0><80><bf>'" "" { target c++23 } }
auto K = U"àŸ€";				// { dg-error "invalid UTF-8 character '<e0><9f><80>'" "" { target c++23 } }
auto L = U"à¿";					// { dg-error "invalid UTF-8 character '<e0><bf>'" "" { target c++23 } }
auto M = U"ì€";					// { dg-error "invalid UTF-8 character '<ec><80>'" "" { target c++23 } }
auto N = U"í €";				// { dg-error "invalid UTF-8 character '<ed><a0><80>'" "" { target c++23 } }
auto O = U"ğ€€€";				// { dg-error "invalid UTF-8 character '<f0><80><80><80>'" "" { target c++23 } }
auto P = U"ğ¿¿";				// { dg-error "invalid UTF-8 character '<f0><8f><bf><bf>'" "" { target c++23 } }
auto Q = U"ô€€";				// { dg-error "invalid UTF-8 character '<f4><90><80><80>'" "" { target c++23 } }
auto R = U"ı¿¿¿¿¿";				// { dg-error "invalid UTF-8 character '<fd><bf><bf><bf>'" "" { target c++23 } }
						// { dg-error "invalid UTF-8 character '<bf>'" "" { target c++23 } .-1 }
auto A1 = UR"(Â€ß¿à €íŸ¿î€€ğ€€ô¿¿)";		// { dg-bogus "invalid UTF-8 character" }
auto B1 = UR"(€)";				// { dg-error "invalid UTF-8 character '<80>'" "" { target c++23 } }
auto C1 = UR"(¿)";				// { dg-error "invalid UTF-8 character '<bf>'" "" { target c++23 } }
auto D1 = UR"(À)";				// { dg-error "invalid UTF-8 character '<c0>'" "" { target c++23 } }
auto E1 = UR"(Á)";				// { dg-error "invalid UTF-8 character '<c1>'" "" { target c++23 } }
auto F1 = UR"(õ)";				// { dg-error "invalid UTF-8 character '<f5>'" "" { target c++23 } }
auto G1 = UR"(ÿ)";				// { dg-error "invalid UTF-8 character '<ff>'" "" { target c++23 } }
auto H1 = UR"(Â)";				// { dg-error "invalid UTF-8 character '<c2>'" "" { target c++23 } }
auto I1 = UR"(à)";				// { dg-error "invalid UTF-8 character '<e0>'" "" { target c++23 } }
auto J1 = UR"(à€¿)";				// { dg-error "invalid UTF-8 character '<e0><80><bf>'" "" { target c++23 } }
auto K1 = UR"(àŸ€)";				// { dg-error "invalid UTF-8 character '<e0><9f><80>'" "" { target c++23 } }
auto L1 = UR"(à¿)";				// { dg-error "invalid UTF-8 character '<e0><bf>'" "" { target c++23 } }
auto M1 = UR"(ì€)";				// { dg-error "invalid UTF-8 character '<ec><80>'" "" { target c++23 } }
auto N1 = UR"(í €)";				// { dg-error "invalid UTF-8 character '<ed><a0><80>'" "" { target c++23 } }
auto O1 = UR"(ğ€€€)";				// { dg-error "invalid UTF-8 character '<f0><80><80><80>'" "" { target c++23 } }
auto P1 = UR"(ğ¿¿)";				// { dg-error "invalid UTF-8 character '<f0><8f><bf><bf>'" "" { target c++23 } }
auto Q1 = UR"(ô€€)";				// { dg-error "invalid UTF-8 character '<f4><90><80><80>'" "" { target c++23 } }
auto R1 = UR"(ı¿¿¿¿¿)";				// { dg-error "invalid UTF-8 character '<fd><bf><bf><bf>'" "" { target c++23 } }
						// { dg-error "invalid UTF-8 character '<bf>'" "" { target c++23 } .-1 }
auto A2 = u8"Â€ß¿à €íŸ¿î€€ğ€€ô¿¿";		// { dg-bogus "invalid UTF-8 character" }
auto B2 = u8"€";				// { dg-error "invalid UTF-8 character '<80>'" "" { target c++23 } }
auto C2 = u8"¿";				// { dg-error "invalid UTF-8 character '<bf>'" "" { target c++23 } }
auto D2 = u8"À";				// { dg-error "invalid UTF-8 character '<c0>'" "" { target c++23 } }
auto E2 = u8"Á";				// { dg-error "invalid UTF-8 character '<c1>'" "" { target c++23 } }
auto F2 = u8"õ";				// { dg-error "invalid UTF-8 character '<f5>'" "" { target c++23 } }
auto G2 = u8"ÿ";				// { dg-error "invalid UTF-8 character '<ff>'" "" { target c++23 } }
auto H2 = u8"Â";				// { dg-error "invalid UTF-8 character '<c2>'" "" { target c++23 } }
auto I2 = u8"à";				// { dg-error "invalid UTF-8 character '<e0>'" "" { target c++23 } }
auto J2 = u8"à€¿";				// { dg-error "invalid UTF-8 character '<e0><80><bf>'" "" { target c++23 } }
auto K2 = u8"àŸ€";				// { dg-error "invalid UTF-8 character '<e0><9f><80>'" "" { target c++23 } }
auto L2 = u8"à¿";				// { dg-error "invalid UTF-8 character '<e0><bf>'" "" { target c++23 } }
auto M2 = u8"ì€";				// { dg-error "invalid UTF-8 character '<ec><80>'" "" { target c++23 } }
auto N2 = u8"í €";				// { dg-error "invalid UTF-8 character '<ed><a0><80>'" "" { target c++23 } }
auto O2 = u8"ğ€€€";				// { dg-error "invalid UTF-8 character '<f0><80><80><80>'" "" { target c++23 } }
auto P2 = u8"ğ¿¿";				// { dg-error "invalid UTF-8 character '<f0><8f><bf><bf>'" "" { target c++23 } }
auto Q2 = u8"ô€€";				// { dg-error "invalid UTF-8 character '<f4><90><80><80>'" "" { target c++23 } }
auto R2 = u8"ı¿¿¿¿¿";				// { dg-error "invalid UTF-8 character '<fd><bf><bf><bf>'" "" { target c++23 } }
						// { dg-error "invalid UTF-8 character '<bf>'" "" { target c++23 } .-1 }
