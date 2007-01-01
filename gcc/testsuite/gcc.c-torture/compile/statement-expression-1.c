/* PR middle-end/30253, We would ICE with statement expressions
   in a conditional expression because we forgot to update the wrapper
   function for the gimple modify statement.  */

#define f(x) ({ unsigned tmp=x; tmp; })

unsigned foo(unsigned x) {
  return __builtin_constant_p(x) ? 0 : f(x);
}

