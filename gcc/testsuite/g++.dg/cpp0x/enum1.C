// PR c++/38021
// { dg-do compile }
// { dg-options "-std=gnu++0x" }

enum : { };	// { dg-error "expected" }
enum : 3 { };	// { dg-error "expected" }
