// { dg-additional-options -fmodules-atom }

#define EXPORT export
EXPORT module bob; // { dg-error "expected" "" { xfail *-*-* } }
