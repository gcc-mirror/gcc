// PR c++/19667

struct integral_constant { };

template<typename _Tp>
struct is_function : public integral_constant { }; // { dg-error "previous" }

template<>
struct is_function : public integral_constant { }; // { dg-error "" }
