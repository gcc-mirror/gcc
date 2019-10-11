// PR c++/67147
// { dg-do compile { target c++2a } }

template <class F, class I1, class I2 = I1>
concept IndirectCallableRelation = true;

template <class, class, class = void>
constexpr bool indirect_relation() { return false; }

// FIXME: The original bug was found using the introducer syntax.

template<typename F, typename I1>
  requires IndirectCallableRelation<F, I1>
constexpr bool indirect_relation() { return true; }

template<typename F, typename I1, typename I2>
  requires IndirectCallableRelation<F, I1, I2>
constexpr bool indirect_relation() { return true; }

// This was added to the discussion thread as a minimum repro.

template<typename T, int = sizeof(T)> 
concept C1 = true;

template <C1 T> int test();
