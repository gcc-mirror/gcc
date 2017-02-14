// { dg-do compile { target c++11 } }

using I = int;			// { dg-message "int" }
using I = float;		// { dg-error "float" }
