// P0847R7
// { dg-do compile { target c++23 } }

// rejection and diagnosis of incorrect uses of 'this' in declarations and definitions

using func_type = void(this int); // { dg-line func_type_line }
// { dg-error "a function type cannot have an explicit object parameter" "" { target *-*-* } func_type_line }
// { dg-note "the type of an explicit object member function is a regular function type" "" { target *-*-* } func_type_line }

using func_ptr_type = void(*)(this int); // { dg-line func_ptr_type_line }
// { dg-error "a pointer to function type cannot have an explicit object parameter" "" { target *-*-* } func_ptr_type_line }
// { dg-note "the type of a pointer to explicit object member function is a regular pointer to function type" "" { target *-*-* } func_ptr_type_line }

struct S {
    static void f(this S) {} // { dg-line static_member_func_line }
};
// { dg-error "an explicit object member function cannot be 'static'" "" { target *-*-* } static_member_func_line }
// { dg-note "explicit object parameter declared here" "" { target *-*-* } static_member_func_line }

using mem_func_type = void (S::*)(this S&); // { dg-line mem_func_type_line }
// { dg-error "a pointer to member function type cannot have an explicit object parameter" "" { target *-*-* } mem_func_type_line }
// { dg-note "the type of a pointer to explicit object member function is a regular pointer to function type" "" { target *-*-* } mem_func_type_line }

void f(this int); // { dg-error "a non-member function cannot have an explicit object parameter" }
void f(this int) {} // { dg-error "a non-member function cannot have an explicit object parameter" }

