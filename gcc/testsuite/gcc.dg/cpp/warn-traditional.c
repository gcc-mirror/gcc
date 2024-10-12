// { dg-do compile }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Wtraditional -Wno-deprecated -Wno-long-long" }

#assert x(x)         // { dg-warning "suggest hiding '#assert' from traditional C with an indented '#' .-Wtraditional." }

 #define X X         // { dg-warning "traditional C ignores '#define' with the '#' indented .-Wtraditional." }

#if 0
#elif 1              // { dg-warning "suggest not using '#elif' in traditional C .-Wtraditional." }
#endif

#define f(X) X
int f;               // { dg-warning "function-like macro 'f' must be used with arguments in traditional C .-Wtraditional." }

#if 0U               // { dg-warning "traditional C rejects the 'U' suffix .-Wtraditional." }
#endif

#if +1               // { dg-warning " traditional C rejects the unary plus operator .-Wtraditional." }
#endif

char *x = "\x0";     // { dg-warning "the meaning of '.x' is different in traditional C .-Wtraditional." }
char *y = "\a";      // { dg-warning "the meaning of '.a' is different in traditional C .-Wtraditional." }
char *z = "\u0F43";  // { dg-warning "the meaning of '.u' is different in traditional C .-Wtraditional." }
