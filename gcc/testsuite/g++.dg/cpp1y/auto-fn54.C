// { dg-do compile { target c++14 } }

using T = int () -> decltype(auto); // { dg-error "function with trailing return type not declared with .auto." }
