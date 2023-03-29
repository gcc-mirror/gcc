// DR 2530 - Multiple definitions of enumerators
// { dg-do compile }

enum E { e, e };		// { dg-error "redefinition of 'e'" }
enum F { f = 0, f = 0 };	// { dg-error "redefinition of 'f'" }
