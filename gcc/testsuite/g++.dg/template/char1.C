template <class CharType, CharType line_terminator = 0>
class String {};

String<signed char, 255> s;		// { dg-error "narrowing conversion" "" { target c++11 } }
// { dg-warning "overflow" "" { target c++98_only } .-1 }
