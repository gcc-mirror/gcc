// PR c++/10779

static void InstantiateConstraint(const float&, unsigned,
                                  void(*AddFunction)(const TYPE&,bool&, // { dg-error "" }
                                                     char*, char*,
                                                     unsigned*)); // { dg-error "" }
