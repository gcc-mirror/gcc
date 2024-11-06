// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Wunused-macros" }

#define X X  // { dg-warning "9:macro 'X' is not used .-Wunused-macros." }
