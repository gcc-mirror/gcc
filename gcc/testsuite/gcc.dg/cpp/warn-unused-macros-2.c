// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Werror=unused-macros" }

#define X X  // { dg-error "macro \"X\" is not used .-Wunused-macros." }
