// Test from P2201R1
// { dg-do compile { target c++11 } }

void f() {

  { auto a = L"" u""; }		// { dg-error "concatenation" }
  { auto a = L"" u8""; }	// { dg-error "concatenation" }
  { auto a = L"" U""; }		// { dg-error "concatenation" }

  { auto a = u8"" L""; }	// { dg-error "concatenation" }
  { auto a = u8"" u""; }	// { dg-error "concatenation" }
  { auto a = u8"" U""; }	// { dg-error "concatenation" }

  { auto a = u"" L""; }		// { dg-error "concatenation" }
  { auto a = u"" u8""; }	// { dg-error "concatenation" }
  { auto a = u"" U""; }		// { dg-error "concatenation" }

  { auto a = U"" L""; }		// { dg-error "concatenation" }
  { auto a = U"" u""; }		// { dg-error "concatenation" }
  { auto a = U"" u8""; }	// { dg-error "concatenation" }
}
