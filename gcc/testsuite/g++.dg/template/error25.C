// PR c++/31923

template<class T>
static void f1 ();

template<>
static void f1<void> ();  // { dg-error "explicit template specialization cannot have a storage class" }

template<class T>
extern void f2 ();

template<>
extern void f2<void> ();  // { dg-error "explicit template specialization cannot have a storage class" }

export template<class T>  // { dg-warning "keyword 'export'" }
static void* f3 ();
