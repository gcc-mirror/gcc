// { dg-do compile }
// Origin: Ewgenij Gawrilow <gawrilow@math.tu-berlin.de>

// PR c++/6723
// ICE when default template argument contains instantiation of
// template template parameter.

template <typename A, typename B,
	  template <typename,typename> class Predicate,
	  bool _matches=Predicate<A,B>::answer>
struct helper { };
