// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -fextended-identifiers -Wnormalized=nfc" }

\u0F43  // { dg-warning "`.U00000f43' is not in NFC .-Wnormalized=." }
