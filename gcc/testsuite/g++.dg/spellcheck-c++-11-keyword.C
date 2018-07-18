/* c++/77922: "constexpr" is only available from C++11 onwards.
   We shouldn't offer it as a spellcheck suggestion in C++98.  */
// { dg-options "-std=c++98" }

constexpr int a = 1; // { dg-bogus "did you mean" }
// { dg-error ".constexpr. does not name a type" "" { target *-*-* } .-1 }
// { dg-message "C\\+\\+11 .constexpr. only available with -std=c\\+\\+11 or -std=gnu\\+\\+11" "" { target *-*-* } .-2 }

/* If the user typos "constexpr" (here as "consexpr"), don't offer it as a
   spelling suggestion in C++98 mode.  */
consexpr int a = 1; // { dg-bogus "did you mean" }
// { dg-error ".consexpr. does not name a type" "" { target *-*-* } .-1 }

decltype i = 0; // { dg-bogus "did you mean" }
// { dg-error ".decltype. does not name a type" "" { target *-*-* } .-1 }
