// Build don't link: 
class A {};
class B : public virtual A {};
template <class Imp> class C : public Imp {};

template class C<B>;
