// { dg-additional-options "-fmodules-ts" }

module foo;

__is_nt_convertible_helper<int, int, false> ok;
__is_nt_convertible_helper<int, int, true> not_ok; // { dg-error "incomplete" }
