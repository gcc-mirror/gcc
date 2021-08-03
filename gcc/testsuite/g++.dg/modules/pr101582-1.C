// PR c++/101582
// { dg-additional-options "-fmodules-ts" }
export module pr101582;
// { dg-module-cmi "pr101582" }
export ;			// { dg-error "export declaration does not declare anything" "" { xfail *-*-* } }
export [[]];			// { dg-error "export declaration does not declare anything" "" { xfail *-*-* } }
export				// { dg-error "export declaration does not declare anything" "" { xfail *-*-* } }
{
}
