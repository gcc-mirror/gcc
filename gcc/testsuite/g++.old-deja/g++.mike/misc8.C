// { dg-do assemble  }
// GROUPS passed vtable
class T { public: virtual ~T() {} };
template<class P> class X : public virtual T {};
int main() { X<int> x; }
