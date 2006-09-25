// PR c++/19667

struct integral_constant { };

template<typename _Tp>
struct is_function : public integral_constant { };

template<>
struct is_function : public integral_constant { }; // { dg-error "" }
