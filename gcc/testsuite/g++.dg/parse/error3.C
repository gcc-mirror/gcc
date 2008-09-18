// { dg-options "-fshow-column" }
// PR c++/10779

static void InstantiateConstraint(const float&, unsigned,
                                  void(*AddFunction)(const TYPE&,bool&,
                                                     char*, char*,
                                                     unsigned*));
// { dg-error "64:expected ',' or '...' before '&' token" "" { target *-*-* }  { 5 } }
// { dg-error "60:ISO C\\+\\+ forbids declaration of 'TYPE' with no type" "" { target *-*-* } { 5 } }
