template <typename Y> struct auto_ptr_ref {
  Y* py;
  auto_ptr_ref(Y* p) : py(p) {}
};
template<typename X> struct auto_ptr {
   X* px;
 public:
   typedef X element_type;

   explicit auto_ptr(X* p =0) throw() : px(p) {}
   auto_ptr(auto_ptr& r) throw() : px(r.release()) {}
   template<typename Y>
      auto_ptr(auto_ptr<Y>& r) throw() : px(r.release()) {}

   auto_ptr& operator=(auto_ptr& r) throw() { 
      reset(r.release()); 
      return *this;
   }
   template<typename Y> auto_ptr& operator=(auto_ptr<Y>& r) throw() { 
      reset(r.release()); 
      return *this;
   }

   ~auto_ptr() { delete px; }

   X& operator*() const throw() { return *px; }
   X* operator->() const throw() { return px; }
   X* get() const throw() { return px; }
   X* release() throw() { X* p=px; px=0; return p; }
   void reset(X* p=0) throw() { if (px != p) delete px, px = p; }

   auto_ptr(auto_ptr_ref<X> r) throw() : px(r.py) {}
   template<typename Y> operator auto_ptr_ref<Y>() throw() {
      return auto_ptr_ref<Y>(release()); 
   }
   template<typename Y> operator auto_ptr<Y>() throw() { 
      return auto_ptr<Y>(release());
   }
};

struct Base { Base() {} virtual ~Base() {} };
struct Derived : Base { Derived() {}; };

auto_ptr<Derived> f() { auto_ptr<Derived> null(0); return null; }
void g(auto_ptr<Derived>) { }
void h(auto_ptr<Base>) { }

int main() {
    auto_ptr<Base> x(f());
    auto_ptr<Derived> y(f());
    x = y;
    g(f());
    h(f());
}
