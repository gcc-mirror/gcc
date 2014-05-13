// PR c++/51199

typedef void FC() const;

template<class T>
struct add_ref {
  typedef T& type;  // { dg-error "forming reference" }
};

typedef add_ref<FC>::type ref_type;

template<class T>
struct add_ptr {
  typedef T* type;  // { dg-error "forming pointer" } 
};

typedef add_ptr<FC>::type ptr_type;
