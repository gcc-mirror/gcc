// { dg-do assemble  }

// by Alexandre Oliva <oliva@dcc.unicamp.br>

// According to [temp.expl.spec]/2, a template explicit specialization
// must be declared in the namespace that contains the declaration of
// the template

namespace N {
  template <class T> class foo;	// { dg-error "" } referenced below
}

using namespace N;

template <> class foo<void>; // { dg-error "" } invalid specialization
