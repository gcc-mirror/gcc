// PR c++/86942
// { dg-do compile { target c++11 } }

using T = auto() -> int;
using U = void() -> int; // { dg-error "11:.type name. function with trailing return type not declared with .auto." }
using W = auto(); // { dg-error "11:.*auto." }
