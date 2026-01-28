// P2246R1
// { dg-do compile { target c++23 } }
// { dg-require-iconv "IBM1047" }
// { dg-options "-pedantic-errors -fexec-charset=IBM1047" }

[[deprecated ("foo")]] int d;		// { dg-message "declared here" }
int e = d;				// { dg-warning "'d' is deprecated: foo" }
static_assert (false, "bar");		// { dg-error "static assertion failed: bar" }
#error "baz"				// { dg-error "#error \"baz\"" }
[[nodiscard ("qux")]] int foo ();	// { dg-message "declared here" }
void
bar ()
{
  foo ();				// { dg-warning "ignoring return value of 'int foo\\\(\\\)', declared with attribute 'nodiscard': 'qux'" }
}
#if __cplusplus > 202302L
#warning "fred"				// { dg-warning "#warning \"fred\"" "" { target c++26 } }
#endif
#if __cpp_static_assert >= 202306L
struct A { constexpr int size () const { return 5; }
           constexpr const char *data () const { return "xyzzy"; } };
static_assert (false, A {});		// { dg-error "static assertion failed: xyzzy" "" { target c++26 } }
#endif
#if __cpp_deleted_function >= 202403L
int baz () = delete ("garply");		// { dg-message "declared here" "" { target c++26 } }
void
plugh ()
{
  baz ();				// { dg-error "use of deleted function 'int baz\\\(\\\)': garply" "" { target c++26 } }
}
#endif
namespace [[deprecated ("corge")]] ND	// { dg-message "declared here" }
{
  int i;
};
int j = ND::i;				// { dg-warning "'ND' is deprecated: corge" }
