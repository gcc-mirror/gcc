// { dg-do assemble  }
// GROUPS passed RTTI
#include <typeinfo>

struct B                                { virtual ~B(){}  };
struct D0 : public virtual B            { virtual ~D0(){}  };
struct D1 : public virtual D0           { virtual ~D1(){}  };
struct C : public virtual B, public D1  { virtual ~C() { } };
