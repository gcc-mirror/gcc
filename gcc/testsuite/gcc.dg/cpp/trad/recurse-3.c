/* Tests that macros that look recursive but are not are accepted.  */

/* { dg-do preprocess } */

#define g(x) x
g(g(g(g(g(g(g))))));	       /* { dg-bogus "detected recursion" } */

/* This macro gets longer with each loop, to thwart tests for
   recursion based on length.  */
#define f(a,b,c,d,e,f,g,h,i) a(b,c,d,e,f,g,h,i,2 3 4 5)
f(f,f,f,f,f,f,f,f,f)	       /* { dg-bogus "detected recursion" } */

/* The above cases should be enough, but this is taken from cccp
   sources so let's try it too.  */
#define foo(x,y) bar (x (y,0), y)
foo (foo, baz);	       /* { dg-bogus "detected recursion" } */

#define mac mac/**/ro
mac		       /* { dg-bogus "detected recursion" } */

#define mac2 mac2
"mac2" 		       /* { dg-bogus "detected recursion" } */

#define macro "macro
macro mac2	       /* { dg-bogus "detected recursion" } */
