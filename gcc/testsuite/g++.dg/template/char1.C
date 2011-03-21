template <class CharType, CharType line_terminator = 0>
class String {};

String<signed char, 255> s;		// { dg-warning "overflow" }
