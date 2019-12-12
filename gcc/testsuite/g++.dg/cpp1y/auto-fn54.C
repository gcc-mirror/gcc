// { dg-do compile { target c++14 } }

using T = int () -> decltype(auto); // { dg-error "11:.type name. function with trailing return type not declared with .auto." }
