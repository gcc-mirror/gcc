// DR 2032 - Default template-arguments of variable templates
// PR c++/96218
// { dg-do compile { target c++14 } }

// [temp.param]/14: If a template-parameter of a class template, variable
// template, or alias template has a default template-argument, each subsequent
// template-parameter shall either have a default template-argument supplied or
// be a template parameter pack.
template<typename T = int, typename U>
T vt; // { dg-error "no default argument" }

// [temp.param]/14: If a template-parameter of a primary class template,
// primary variable template, or alias template is a template parameter pack,
// it shall be the last template-parameter.
template<typename... Ts, typename U> // { dg-error "must be at the end" }
int vt2;
