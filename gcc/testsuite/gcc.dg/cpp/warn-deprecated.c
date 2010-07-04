// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Wdeprecated" }

#assert x(x)  // { dg-warning "#assert is a deprecated GCC extension .-Wdeprecated." }

#if #x(x)     // { dg-warning "assertions are a deprecated extension .-Wdeprecated." }
#endif
