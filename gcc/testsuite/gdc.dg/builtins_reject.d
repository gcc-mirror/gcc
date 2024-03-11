// { dg-do compile }

import gcc.builtins;

auto test_sqrt() { return &__builtin_sqrt; }
auto test_tan() { return &__builtin_tan; }
auto test_malloc() { return &__builtin_malloc; }
auto test_printf() { return &__builtin_printf; }

// { dg-error ".__builtin_inf. must be directly called" "" { target *-*-* } .+1 }
auto test_inf() { return &__builtin_inf; }
// { dg-error ".__builtin_lfloor. must be directly called" "" { target *-*-* } .+1 }
auto test_lfloor() { return &__builtin_lfloor; }
// { dg-error ".__builtin_setjmp. must be directly called" "" { target *-*-* } .+1 }
auto test_setjmp() { return &__builtin_setjmp; }
// { dg-error ".__builtin_unreachable. must be directly called" "" { target *-*-* } .+1 }
auto test_unreachable() { return &__builtin_unreachable; }
