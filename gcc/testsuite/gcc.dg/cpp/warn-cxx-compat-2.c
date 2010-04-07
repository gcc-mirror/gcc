// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Werror=c++-compat" }

#define not !  // { dg-error "identifier \"not\" is a special operator name in C\\+\\+ .-Wc\\+\\+-compat." }
