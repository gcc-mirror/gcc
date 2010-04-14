// { dg-do compile }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Werror=traditional -Wno-deprecated -Wno-long-long" }
/* { dg-message "some warnings being treated as errors" "" {target "*-*-*"} 0 } */
#assert x(x)         // { dg-error "suggest hiding #assert from traditional C with an indented # .-Werror=traditional." }

 #define X X         // { dg-error "traditional C ignores #define with the # indented .-Werror=traditional." }

#if 0
#elif 1              // { dg-error "suggest not using #elif in traditional C .-Werror=traditional." }
#endif

#define f(X) X
int f;               // { dg-error "function-like macro \"f\" must be used with arguments in traditional C .-Werror=traditional." }

#if 0U               // { dg-error "traditional C rejects the \"U\" suffix .-Werror=traditional." }
#endif

#if +1               // { dg-error " traditional C rejects the unary plus operator .-Werror=traditional." }
#endif

char *x = "\x0";     // { dg-error "the meaning of '.x' is different in traditional C .-Werror=traditional." }
char *y = "\a";      // { dg-error "the meaning of '.a' is different in traditional C .-Werror=traditional." }
char *z = "\u0F43";  // { dg-error "the meaning of '.u' is different in traditional C .-Werror=traditional." }
