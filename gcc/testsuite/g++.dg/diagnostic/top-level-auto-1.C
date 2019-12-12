// { dg-do compile { target c++98_only } }

auto int i;  // { dg-error "1:top-level declaration of .i. specifies .auto." }
