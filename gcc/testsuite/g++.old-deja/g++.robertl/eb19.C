// Build don't link:

#define CRASH 1
#ifdef CRASH
#define VIRTUAL virtual
#else
#define VIRTUAL
#endif

class A {};
class B : public VIRTUAL A {};
template <class Imp> class C : public /*virtual*/ Imp {};
// define CRASH and uncomment here    ^^^^^^^^^^^
// and the crash goes away!!!!

template class C<B>;
