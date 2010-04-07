// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Wtraditional -Wlong-long" }

#if 0LL  // { dg-warning "traditional C rejects the \"LL\" suffix .-Wlong-long." }
         // { dg-warning "use of C99 long long integer constant .-Wlong-long." "use long long" { target *-*-* } 4 }
#endif
