// { dg-options "-fshow-column" }
// PR c++/10779

static void InstantiateConstraint(const float&, unsigned,
                                  void(*AddFunction)(const TYPE&,bool&,
                                                     char*, char*,
                                                     unsigned*));
// { dg-error "64: ISO C\\+\\+ forbids declaration of 'parameter' with no type" "forbids" { target *-*-* }  { 5 } }
// { dg-error "60: 'TYPE' does not name a type" "does not" { target *-*-* } { 5 } }
