// PR c++/38021
// { dg-do compile }
// { dg-options "-std=gnu++11" }

enum : { };	// { dg-error "expected" }
enum : 3 { };	// { dg-error "expected" }
