// { dg-additional-options -fmodules-atom }

#define EXPORT export // { dg-error "" }
EXPORT module bob; // { dg-error "" }
