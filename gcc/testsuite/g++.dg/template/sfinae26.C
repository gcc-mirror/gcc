// Origin: PR c++/46170
// { dg-do compile }

namespace util {
  struct option_value {
  };
  template <class T> struct options_map_impl {
    typedef T options_struct_type;
    typedef bool (*opt_func)(const option_value&, options_struct_type&);
    template <class V, V K>  static  bool  set_member_constant(const option_value&,
							       options_struct_type&, V options_struct_type::*);
    template <class V, V options_struct_type::*mem, V K>  static  bool 
    set_member_constant(const option_value& opt, options_struct_type& t) {
      return set_member_constant<V,K>(opt, t, mem);
    }
  };
}
struct cflat_options {
  bool show_precharges;
};
typedef util::options_map_impl<cflat_options> options_map_impl_type;
class register_options_modifier {
  typedef options_map_impl_type::opt_func modifier_type;
public:  register_options_modifier();
  register_options_modifier(const char* Mode,    const modifier_type COM,   
			    const char* h);
};
static const register_options_modifier
cflat_opt_mod_show_precharges("precharges",
			      &options_map_impl_type::set_member_constant<bool,
									  &cflat_options::show_precharges, true>, "show precharge expressions"),
  cflat_opt_mod_no_show_precharges("no-" "precharges",
				   &options_map_impl_type::set_member_constant<bool,
									       &cflat_options::show_precharges, false>, "hide precharge expressions");
