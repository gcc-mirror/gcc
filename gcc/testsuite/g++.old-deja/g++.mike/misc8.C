// Build don't link: 
// GROUPS passed vtable
class T { public: virtual ~T() {} };
template<class P> class X : public virtual T {};
main() { X<int> x; }
