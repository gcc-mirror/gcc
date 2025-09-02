// PR c++/120845
// { dg-do compile }
// { dg-additional-options "-fmodules" }

export module pr120485
    [[foobarbaz]];
// { dg-error "expected ';' before end of line" "" { target *-*-* } .-2 }
// { dg-warning "attribute ignored" "" { target *-*-* } .-2 }
