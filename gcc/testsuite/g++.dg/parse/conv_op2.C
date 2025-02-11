// PR c++/118306
// { dg-do "compile" }

struct K {
  char operator int();	// { dg-error "return type specified for" }
  * operator short();	// { dg-error "return type specified for" }
  ** operator float();	// { dg-error "return type specified for" }
  &* operator double();	// { dg-error "return type specified for|pointer to 'double&'" }
  & operator long();	// { dg-error "return type specified for" }
};
