// PR c++/52671
// { dg-do compile }
__attribute__ ((deprecated)) enum E { E0 };	// { dg-warning "attribute ignored in declaration of" "ignored" }
// { dg-message "must follow the" "must follow" { target *-*-* } .-1 }
