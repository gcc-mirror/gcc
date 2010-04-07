// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Werror=deprecated" }

#assert x(x)  // { dg-error "#assert is a deprecated GCC extension .-Wdeprecated." }

#if #x(x)     // { dg-error "assertions are a deprecated extension .-Wdeprecated." }
#endif
