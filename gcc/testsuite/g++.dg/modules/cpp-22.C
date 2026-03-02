// PR c++/124153
// { dg-additional-options "-fmodules -fdirectives-only -E" }

// directives-only mode currently does not do error checking.
export module hello  // { dg-error "expected ';'" "" { xfail *-*-* } }
import world;
