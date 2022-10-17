// { dg-do preprocess { target { c++11 } } }
// { dg-options "-pedantic-errors" }

\u00AA
\u00B7	// { dg-error "is not valid at the start of an identifier" }
\u0F43  // { dg-error "not in NFC" }
a\u05B8\u05B9\u05B9\u05BBb
 a\u05BB\u05B9\u05B8\u05B9b  // { dg-error "not in NFC" }
\u09CB	// { dg-error "is not valid at the start of an identifier" }
\u09C7\u09BE // { dg-error "not in NFC" }
	// { dg-error "is not valid at the start of an identifier" "" { target *-*-* } .-1 }
\u0B4B	// { dg-error "is not valid at the start of an identifier" }
\u0B47\u0B3E // { dg-error "not in NFC" }
	// { dg-error "is not valid at the start of an identifier" "" { target *-*-* } .-1 }
\u0BCA	// { dg-error "is not valid at the start of an identifier" }
\u0BC6\u0BBE // { dg-error "not in NFC" }
	// { dg-error "is not valid at the start of an identifier" "" { target *-*-* } .-1 }
\u0BCB	// { dg-error "is not valid at the start of an identifier" }
\u0BC7\u0BBE // { dg-error "not in NFC" }
	// { dg-error "is not valid at the start of an identifier" "" { target *-*-* } .-1 }
\u0CCA	// { dg-error "is not valid at the start of an identifier" }
\u0CC6\u0CC2 // { dg-error "not in NFC" }
	// { dg-error "is not valid at the start of an identifier" "" { target *-*-* } .-1 }
\u0D4A	// { dg-error "is not valid at the start of an identifier" }
\u0D46\u0D3E // { dg-error "not in NFC" }
	// { dg-error "is not valid at the start of an identifier" "" { target *-*-* } .-1 }
\u0D4B	// { dg-error "is not valid at the start of an identifier" }
\u0D47\u0D3E // { dg-error "not in NFC" }
	// { dg-error "is not valid at the start of an identifier" "" { target *-*-* } .-1 }

K
\u212A // { dg-error "not in NFC" }

\u03AC
\u1F71 // { dg-error "not in NFC" }

\uAC00
\u1100\u1161 // { dg-error "not in NFC" }
\uAC01
\u1100\u1161\u11A8 // { dg-error "not in NFC" }
\uAC00\u11A8 // { dg-error "not in NFC" }

ª
·	// { dg-error "is not valid at the start of an identifier" }
གྷ  // { dg-error "not in NFC" }
aָֹֹֻb
 aָֹֹֻb  // { dg-error "not in NFC" }
ো	// { dg-error "is not valid at the start of an identifier" }
ো // { dg-error "not in NFC" }
	// { dg-error "is not valid at the start of an identifier" "" { target *-*-* } .-1 }
ୋ	// { dg-error "is not valid at the start of an identifier" }
ୋ // { dg-error "not in NFC" }
	// { dg-error "is not valid at the start of an identifier" "" { target *-*-* } .-1 }
ொ	// { dg-error "is not valid at the start of an identifier" }
ொ // { dg-error "not in NFC" }
	// { dg-error "is not valid at the start of an identifier" "" { target *-*-* } .-1 }
ோ	// { dg-error "is not valid at the start of an identifier" }
ோ // { dg-error "not in NFC" }
	// { dg-error "is not valid at the start of an identifier" "" { target *-*-* } .-1 }
ೊ	// { dg-error "is not valid at the start of an identifier" }
ೊ // { dg-error "not in NFC" }
	// { dg-error "is not valid at the start of an identifier" "" { target *-*-* } .-1 }
ൊ	// { dg-error "is not valid at the start of an identifier" }
ൊ // { dg-error "not in NFC" }
	// { dg-error "is not valid at the start of an identifier" "" { target *-*-* } .-1 }
ോ	// { dg-error "is not valid at the start of an identifier" }
ോ // { dg-error "not in NFC" }
	// { dg-error "is not valid at the start of an identifier" "" { target *-*-* } .-1 }

K
K // { dg-error "not in NFC" }

ά
ά // { dg-error "not in NFC" }

가
가 // { dg-error "not in NFC" }
각
각 // { dg-error "not in NFC" }
각 // { dg-error "not in NFC" }
