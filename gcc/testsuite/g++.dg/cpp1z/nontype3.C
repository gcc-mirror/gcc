// { dg-do compile { target c++17 } }

#ifndef __cpp_nontype_template_args
#error __cpp_nontype_template_args not defined
#endif

#if __cpp_nontype_template_args != 201411
#error Wrong value for __cpp_nontype_template_args
#endif
