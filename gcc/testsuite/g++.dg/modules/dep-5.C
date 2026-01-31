// { dg-additional-options "-fmodules -M" }

export module A.B:X.Y;
export module A.B:X.Y;  // { dg-error "module already declared" }
export module F;	// { dg-error "module already declared" }
