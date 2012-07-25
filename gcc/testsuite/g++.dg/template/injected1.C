// PR c++/13950, DR 176

template <class T> struct Base { }; // { dg-message "" } candidate

struct D1: Base<void>
{
  D1::Base* p1;
  D1::Base<double>* p2;
  Base *p3;
  Base<double>* p4;
};

struct D2: Base<void>, Base<void*>
{
  D2::Base* p1;			// { dg-error "" }
  D2::Base<double>* p2;
  Base *p3;			// { dg-error "" }
  Base<double>* p4;
};

template <class T>
struct D3: Base<T> {
  typename D3::Base* p1;
  typename D3::template Base<double>* p2;
};
template struct D3<void>;

template <class T>
struct D4: Base<T>, Base<T*> {
  typename D4::Base* p1;	// { dg-error "" }
  typename D4::template Base<double>* p2;
};
template struct D4<void>;	// { dg-message "required" }
