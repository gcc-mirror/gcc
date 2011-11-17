// PR c++/51186

auto main()->int	       // { dg-error "std=" "" { target c++98 } }
			       // { dg-error "auto" "" { target c++98 } 3 }
			       // { dg-error "no type" "" { target c++98 } 3 }
{ }
