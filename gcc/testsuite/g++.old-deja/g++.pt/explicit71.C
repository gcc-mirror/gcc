// { dg-do assemble  }
// by Alexandre Oliva <oliva@dcc.unicamp.br>
// Based on a testcase by Reid M. Pinchback <reidmp@MIT.EDU>
// According to the C++ Standard [temp.expl.spec]/17-18, explicit
// specializations are only valid if all enclosing template classes
// of the specialized template are fully specialized too

template <class X> 
class bug {
  template <class Y> 
  class a {}; 
};
template <class X> 
template <>			// { dg-error "" } invalid specialization
class bug<X>::a<char> {};	// { dg-error "" }
