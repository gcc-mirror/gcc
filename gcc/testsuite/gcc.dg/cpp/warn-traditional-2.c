// { dg-do compile }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Werror=traditional -Wno-deprecated -Wno-long-long" }

#assert x(x)         // { dg-error "suggest hiding #assert from traditional C with an indented # .-Wtraditional." }

 #define X X         // { dg-error "traditional C ignores #define with the # indented .-Wtraditional." }

#if 0
#elif 1              // { dg-error "suggest not using #elif in traditional C .-Wtraditional." }
#endif

#define f(X) X
int f;               // { dg-error "function-like macro \"f\" must be used with arguments in traditional C .-Wtraditional." }

#if 0U               // { dg-error "traditional C rejects the \"U\" suffix .-Wtraditional." }
#endif

#if +1               // { dg-error " traditional C rejects the unary plus operator .-Wtraditional." }
#endif

char *x = "\x0";     // { dg-error "the meaning of '.x' is different in traditional C .-Wtraditional." }
char *y = "\a";      // { dg-error "the meaning of '.a' is different in traditional C .-Wtraditional." }
char *z = "\u0F43";  // { dg-error "the meaning of '.u' is different in traditional C .-Wtraditional." }
