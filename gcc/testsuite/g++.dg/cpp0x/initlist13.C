// PR c++/39056
// { dg-do compile }
// { dg-options "-std=gnu++0x" }

__complex__ int i ({0});	// { dg-error "cannot convert" }
