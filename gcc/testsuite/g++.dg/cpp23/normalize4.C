// { dg-do preprocess { target { c++11 } } }
// { dg-options "" }

\u00AA
x\u00B7
\u0F43  // { dg-warning "not in NFC" }
a\u05B8\u05B9\u05B9\u05BBb
 a\u05BB\u05B9\u05B8\u05B9b  // { dg-warning "not in NFC" }
x\u09CB
x\u09C7\u09BE // { dg-warning "not in NFC" }
x\u0B4B
x\u0B47\u0B3E // { dg-warning "not in NFC" }
x\u0BCA
x\u0BC6\u0BBE // { dg-warning "not in NFC" }
x\u0BCB
x\u0BC7\u0BBE // { dg-warning "not in NFC" }
x\u0CCA
x\u0CC6\u0CC2 // { dg-warning "not in NFC" }
x\u0D4A
x\u0D46\u0D3E // { dg-warning "not in NFC" }
x\u0D4B
x\u0D47\u0D3E // { dg-warning "not in NFC" }

K
\u212A // { dg-warning "not in NFC" }

\u03AC
\u1F71 // { dg-warning "not in NFC" }

\uAC00
\u1100\u1161 // { dg-warning "not in NFC" }
\uAC01
\u1100\u1161\u11A8 // { dg-warning "not in NFC" }
\uAC00\u11A8 // { dg-warning "not in NFC" }

ª
x·
གྷ  // { dg-warning "not in NFC" }
aָֹֹֻb
 aָֹֹֻb  // { dg-warning "not in NFC" }
xো
xো // { dg-warning "not in NFC" }
xୋ
xୋ // { dg-warning "not in NFC" }
xொ
xொ // { dg-warning "not in NFC" }
xோ
xோ // { dg-warning "not in NFC" }
xೊ
xೊ // { dg-warning "not in NFC" }
xൊ
xൊ // { dg-warning "not in NFC" }
xോ
xോ // { dg-warning "not in NFC" }

K
K // { dg-warning "not in NFC" }

ά
ά // { dg-warning "not in NFC" }

가
가 // { dg-warning "not in NFC" }
각
각 // { dg-warning "not in NFC" }
각 // { dg-warning "not in NFC" }
