// P2295R6 - Support for UTF-8 as a portable source file encoding
// This test intentionally contains various byte sequences which are not valid UTF-8
// { dg-do preprocess { target c++11 } }
// { dg-options "-finput-charset=UTF-8 -pedantic-errors -Wno-invalid-utf8" }

char32_t a = U'�';				// { dg-bogus "invalid UTF-8 character <80>" "" { target c++23 } }
char32_t b = U'�';				// { dg-bogus "invalid UTF-8 character <bf>" "" { target c++23 } }
char32_t c = U'�';				// { dg-bogus "invalid UTF-8 character <c0>" "" { target c++23 } }
char32_t d = U'�';				// { dg-bogus "invalid UTF-8 character <c1>" "" { target c++23 } }
char32_t e = U'�';				// { dg-bogus "invalid UTF-8 character <f5>" "" { target c++23 } }
char32_t f = U'�';				// { dg-bogus "invalid UTF-8 character <ff>" "" { target c++23 } }
char32_t g = U'�';				// { dg-bogus "invalid UTF-8 character <c2>" "" { target c++23 } }
char32_t h = U'�';				// { dg-bogus "invalid UTF-8 character <e0>" "" { target c++23 } }
char32_t i = U'���';				// { dg-bogus "invalid UTF-8 character <e0><80><bf>" "" { target c++23 } }
char32_t j = U'���';				// { dg-bogus "invalid UTF-8 character <e0><9f><80>" "" { target c++23 } }
char32_t k = U'�';				// { dg-bogus "invalid UTF-8 character <e0><bf>" "" { target c++23 } }
char32_t l = U'�';				// { dg-bogus "invalid UTF-8 character <ec><80>" "" { target c++23 } }
char32_t m = U'�';				// { dg-bogus "invalid UTF-8 character <ed><a0><80>" "" { target c++23 } }
char32_t n = U'����';				// { dg-bogus "invalid UTF-8 character <f0><80><80><80>" "" { target c++23 } }
char32_t o = U'����';				// { dg-bogus "invalid UTF-8 character <f0><8f><bf><bf>" "" { target c++23 } }
char32_t p = U'����';				// { dg-bogus "invalid UTF-8 character <f4><90><80><80>" "" { target c++23 } }
char32_t q = U'������';				// { dg-bogus "invalid UTF-8 character <fd><bf><bf><bf>" "" { target c++23 } }
						// { dg-bogus "invalid UTF-8 character <bf>" "" { target c++23 } .-1 }
auto A = U"߿ࠀ퟿𐀀􏿿";		// { dg-bogus "invalid UTF-8 character" }
auto B = U"�";					// { dg-bogus "invalid UTF-8 character <80>" "" { target c++23 } }
auto C = U"�";					// { dg-bogus "invalid UTF-8 character <bf>" "" { target c++23 } }
auto D = U"�";					// { dg-bogus "invalid UTF-8 character <c0>" "" { target c++23 } }
auto E = U"�";					// { dg-bogus "invalid UTF-8 character <c1>" "" { target c++23 } }
auto F = U"�";					// { dg-bogus "invalid UTF-8 character <f5>" "" { target c++23 } }
auto G = U"�";					// { dg-bogus "invalid UTF-8 character <ff>" "" { target c++23 } }
auto H = U"�";					// { dg-bogus "invalid UTF-8 character <c2>" "" { target c++23 } }
auto I = U"�";					// { dg-bogus "invalid UTF-8 character <e0>" "" { target c++23 } }
auto J = U"���";				// { dg-bogus "invalid UTF-8 character <e0><80><bf>" "" { target c++23 } }
auto K = U"���";				// { dg-bogus "invalid UTF-8 character <e0><9f><80>" "" { target c++23 } }
auto L = U"�";					// { dg-bogus "invalid UTF-8 character <e0><bf>" "" { target c++23 } }
auto M = U"�";					// { dg-bogus "invalid UTF-8 character <ec><80>" "" { target c++23 } }
auto N = U"�";				// { dg-bogus "invalid UTF-8 character <ed><a0><80>" "" { target c++23 } }
auto O = U"����";				// { dg-bogus "invalid UTF-8 character <f0><80><80><80>" "" { target c++23 } }
auto P = U"����";				// { dg-bogus "invalid UTF-8 character <f0><8f><bf><bf>" "" { target c++23 } }
auto Q = U"����";				// { dg-bogus "invalid UTF-8 character <f4><90><80><80>" "" { target c++23 } }
auto R = U"������";				// { dg-bogus "invalid UTF-8 character <fd><bf><bf><bf>" "" { target c++23 } }
						// { dg-bogus "invalid UTF-8 character <bf>" "" { target c++23 } .-1 }
auto A1 = UR"(߿ࠀ퟿𐀀􏿿)";		// { dg-bogus "invalid UTF-8 character" }
auto B1 = UR"(�)";				// { dg-bogus "invalid UTF-8 character <80>" "" { target c++23 } }
auto C1 = UR"(�)";				// { dg-bogus "invalid UTF-8 character <bf>" "" { target c++23 } }
auto D1 = UR"(�)";				// { dg-bogus "invalid UTF-8 character <c0>" "" { target c++23 } }
auto E1 = UR"(�)";				// { dg-bogus "invalid UTF-8 character <c1>" "" { target c++23 } }
auto F1 = UR"(�)";				// { dg-bogus "invalid UTF-8 character <f5>" "" { target c++23 } }
auto G1 = UR"(�)";				// { dg-bogus "invalid UTF-8 character <ff>" "" { target c++23 } }
auto H1 = UR"(�)";				// { dg-bogus "invalid UTF-8 character <c2>" "" { target c++23 } }
auto I1 = UR"(�)";				// { dg-bogus "invalid UTF-8 character <e0>" "" { target c++23 } }
auto J1 = UR"(���)";				// { dg-bogus "invalid UTF-8 character <e0><80><bf>" "" { target c++23 } }
auto K1 = UR"(���)";				// { dg-bogus "invalid UTF-8 character <e0><9f><80>" "" { target c++23 } }
auto L1 = UR"(�)";				// { dg-bogus "invalid UTF-8 character <e0><bf>" "" { target c++23 } }
auto M1 = UR"(�)";				// { dg-bogus "invalid UTF-8 character <ec><80>" "" { target c++23 } }
auto N1 = UR"(�)";				// { dg-bogus "invalid UTF-8 character <ed><a0><80>" "" { target c++23 } }
auto O1 = UR"(����)";				// { dg-bogus "invalid UTF-8 character <f0><80><80><80>" "" { target c++23 } }
auto P1 = UR"(����)";				// { dg-bogus "invalid UTF-8 character <f0><8f><bf><bf>" "" { target c++23 } }
auto Q1 = UR"(����)";				// { dg-bogus "invalid UTF-8 character <f4><90><80><80>" "" { target c++23 } }
auto R1 = UR"(������)";				// { dg-bogus "invalid UTF-8 character <fd><bf><bf><bf>" "" { target c++23 } }
						// { dg-bogus "invalid UTF-8 character <bf>" "" { target c++23 } .-1 }
auto A2 = u8"߿ࠀ퟿𐀀􏿿";		// { dg-bogus "invalid UTF-8 character" }
auto B2 = u8"�";				// { dg-bogus "invalid UTF-8 character <80>" "" { target c++23 } }
auto C2 = u8"�";				// { dg-bogus "invalid UTF-8 character <bf>" "" { target c++23 } }
auto D2 = u8"�";				// { dg-bogus "invalid UTF-8 character <c0>" "" { target c++23 } }
auto E2 = u8"�";				// { dg-bogus "invalid UTF-8 character <c1>" "" { target c++23 } }
auto F2 = u8"�";				// { dg-bogus "invalid UTF-8 character <f5>" "" { target c++23 } }
auto G2 = u8"�";				// { dg-bogus "invalid UTF-8 character <ff>" "" { target c++23 } }
auto H2 = u8"�";				// { dg-bogus "invalid UTF-8 character <c2>" "" { target c++23 } }
auto I2 = u8"�";				// { dg-bogus "invalid UTF-8 character <e0>" "" { target c++23 } }
auto J2 = u8"���";				// { dg-bogus "invalid UTF-8 character <e0><80><bf>" "" { target c++23 } }
auto K2 = u8"���";				// { dg-bogus "invalid UTF-8 character <e0><9f><80>" "" { target c++23 } }
auto L2 = u8"�";				// { dg-bogus "invalid UTF-8 character <e0><bf>" "" { target c++23 } }
auto M2 = u8"�";				// { dg-bogus "invalid UTF-8 character <ec><80>" "" { target c++23 } }
auto N2 = u8"�";				// { dg-bogus "invalid UTF-8 character <ed><a0><80>" "" { target c++23 } }
auto O2 = u8"����";				// { dg-bogus "invalid UTF-8 character <f0><80><80><80>" "" { target c++23 } }
auto P2 = u8"����";				// { dg-bogus "invalid UTF-8 character <f0><8f><bf><bf>" "" { target c++23 } }
auto Q2 = u8"����";				// { dg-bogus "invalid UTF-8 character <f4><90><80><80>" "" { target c++23 } }
auto R2 = u8"������";				// { dg-bogus "invalid UTF-8 character <fd><bf><bf><bf>" "" { target c++23 } }
						// { dg-bogus "invalid UTF-8 character <bf>" "" { target c++23 } .-1 }
