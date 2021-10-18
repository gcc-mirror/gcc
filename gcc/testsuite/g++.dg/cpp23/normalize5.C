// { dg-do preprocess { target { c++23 } } }
// { dg-options "-pedantic-errors" }

\u00AA
x\u00B7
\u0F43  // { dg-error "not in NFC" }
a\u05B8\u05B9\u05B9\u05BBb
 a\u05BB\u05B9\u05B8\u05B9b  // { dg-error "not in NFC" }
x\u09CB
x\u09C7\u09BE // { dg-error "not in NFC" }
x\u0B4B
x\u0B47\u0B3E // { dg-error "not in NFC" }
x\u0BCA
x\u0BC6\u0BBE // { dg-error "not in NFC" }
x\u0BCB
x\u0BC7\u0BBE // { dg-error "not in NFC" }
x\u0CCA
x\u0CC6\u0CC2 // { dg-error "not in NFC" }
x\u0D4A
x\u0D46\u0D3E // { dg-error "not in NFC" }
x\u0D4B
x\u0D47\u0D3E // { dg-error "not in NFC" }

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
x·
གྷ  // { dg-error "not in NFC" }
aָֹֹֻb
 aָֹֹֻb  // { dg-error "not in NFC" }
xো
xো // { dg-error "not in NFC" }
xୋ
xୋ // { dg-error "not in NFC" }
xொ
xொ // { dg-error "not in NFC" }
xோ
xோ // { dg-error "not in NFC" }
xೊ
xೊ // { dg-error "not in NFC" }
xൊ
xൊ // { dg-error "not in NFC" }
xോ
xോ // { dg-error "not in NFC" }

K
K // { dg-error "not in NFC" }

ά
ά // { dg-error "not in NFC" }

가
가 // { dg-error "not in NFC" }
각
각 // { dg-error "not in NFC" }
각 // { dg-error "not in NFC" }
