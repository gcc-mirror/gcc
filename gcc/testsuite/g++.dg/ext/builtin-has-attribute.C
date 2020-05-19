// PR c++/90915
// { dg-do compile { target c++11 } }

template<typename T>
void foo ()
{
  static_assert(!__builtin_has_attribute(T::a, aligned), ""); // { dg-message "sorry, unimplemented: .__builtin_has_attribute. with dependent argument not supported yet" }
}
