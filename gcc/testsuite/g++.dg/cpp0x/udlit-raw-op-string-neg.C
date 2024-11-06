// { dg-do compile { target c++11 } }

//  Make sure handing a string to a raw literal generates a sensible error message.

int operator ""_embedraw(const char*)
{ return 41; }

int k = "Boo!"_embedraw;  //  { dg-error "unable to find string literal operator" }
