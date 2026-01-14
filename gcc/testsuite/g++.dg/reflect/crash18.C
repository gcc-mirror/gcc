// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

// We also test this elsewhere but we only crashed when there were no other
// errors.
void fn (decltype(^^::)) {} // { dg-error "function of consteval-only type must be declared .consteval." }
