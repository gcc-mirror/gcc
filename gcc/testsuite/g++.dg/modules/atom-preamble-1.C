// { dg-additional-options -fmodules-atom }

#define EXPORT export // { dg-message "ended here" }
EXPORT module bob; // { dg-error "must be first declaration of" }
