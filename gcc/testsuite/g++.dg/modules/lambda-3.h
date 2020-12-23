
template<int I> inline constexpr auto tmpl = [] {return I;};

inline const auto tpl_1 = tmpl<1>;
inline const auto tpl_2 = tmpl<2>;
