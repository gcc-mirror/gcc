// { dg-options "-fdiagnostics-show-caret" }

/* Various tests of name lookup within a namespace, both within an explicitly
   given namespace, or implicitly.  */

namespace detail {
  /* Various things to look for.  */

  typedef int some_typedef;

  int _foo(int i) { return i; }

  template <typename T>
  T something_else (T i) { return i; }
}

/* Tests of lookup of a typedef.  */

void fn_1_explicit ()
{
  detail::some_type i; // { dg-error ".some_type. is not a member of .detail.; did you mean 'some_typedef'\\?" }
  /* { dg-begin-multiline-output "" }
   detail::some_type i;
           ^~~~~~~~~
           some_typedef
     { dg-end-multiline-output "" } */
}

namespace detail {

void fn_1_implicit ()
{
  some_type i; // { dg-error ".some_type. was not declared in this scope; did you mean 'some_typedef'\\?" }
  /* { dg-begin-multiline-output "" }
   some_type i;
   ^~~~~~~~~
   some_typedef
     { dg-end-multiline-output "" } */
}

} // namespace detail


/* Tests of lookup of a function.  */

void fn_2_explicit (int i) {
  detail::foo(i); // { dg-error ".foo. is not a member of .detail.; did you mean '_foo'\\?" }
  /* { dg-begin-multiline-output "" }
   detail::foo(i);
           ^~~
           _foo
     { dg-end-multiline-output "" } */
}

namespace detail {

void fn_2_implicit (int i) {
  foo(i); // { dg-error ".foo. was not declared in this scope; did you mean '_foo'\\?" }
  /* { dg-begin-multiline-output "" }
   foo(i);
   ^~~
   _foo
     { dg-end-multiline-output "" } */
}

} // namespace detail


/* Examples using a template.  */

void fn_3_explicit (int i) {
  detail::something_els(i); // { dg-error ".something_els. is not a member of .detail.; did you mean 'something_else'\\?" }
  /* { dg-begin-multiline-output "" }
   detail::something_els(i);
           ^~~~~~~~~~~~~
           something_else
     { dg-end-multiline-output "" } */
}

namespace detail {

void fn_3_implicit (int i) {
  something_els(i); // { dg-error ".something_els. was not declared in this scope; did you mean 'something_else'\\?" }
  /* { dg-begin-multiline-output "" }
   something_els(i);
   ^~~~~~~~~~~~~
   something_else
     { dg-end-multiline-output "" } */
}

} // namespace detail


/* Tests of lookup for which no hint is available.  */

void fn_4_explicit (int i) {
  detail::not_recognized(i); // { dg-error ".not_recognized. is not a member of .detail." }
  /* { dg-begin-multiline-output "" }
   detail::not_recognized(i);
           ^~~~~~~~~~~~~~
     { dg-end-multiline-output "" } */
}

namespace detail {

void fn_4_implicit (int i)
{
  not_recognized(i); // { dg-error ".not_recognized. was not declared in this scope" }
  /* { dg-begin-multiline-output "" }
   not_recognized(i);
   ^~~~~~~~~~~~~~
     { dg-end-multiline-output "" } */
}

} // namespace detail


/* Test for failed lookup explicitly within global namespace.  */

typedef int another_typedef;

void fn_5 ()
{
  ::another_type i; // { dg-error ".::another_type. has not been declared; did you mean 'another_typedef'\\?" }
  /* { dg-begin-multiline-output "" }
   ::another_type i;
     ^~~~~~~~~~~~
     another_typedef
     { dg-end-multiline-output "" } */
}
