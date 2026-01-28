// P2246R1
// { dg-do compile { target c++23 } }
// { dg-require-iconv "UTF-8" }
// { dg-options "-pedantic-errors -fexec-charset=UTF-8" }

[[deprecated ("áæ)")]] int d;		// { dg-message "declared here" }
int e = d;				// { dg-warning "'d' is deprecated: áæ" }
static_assert (false, "áæ");		// { dg-error "static assertion failed: áæ" }
#error "áæ"				// { dg-error "#error \"áæ\"" }
[[nodiscard ("áæ")]] int foo ();	// { dg-message "declared here" }
void
bar ()
{
  foo ();				// { dg-warning "ignoring return value of 'int foo\\\(\\\)', declared with attribute 'nodiscard': 'áæ'" }
}
#if __cplusplus > 202302L
#warning "áæ"				// { dg-warning "#warning \"áæ\"" "" { target c++26 } }
#endif
#if __cpp_static_assert >= 202306L
struct A { constexpr int size () const { return sizeof ("áæ") - 1; }
           constexpr const char *data () const { return "áæ"; } };
static_assert (false, A {});		// { dg-error "static assertion failed: áæ" "" { target c++26 } }
#endif
#if __cpp_deleted_function >= 202403L
int baz () = delete ("áæ");		// { dg-message "declared here" "" { target c++26 } }
void
plugh ()
{
  baz ();				// { dg-error "use of deleted function 'int baz\\\(\\\)': áæ" "" { target c++26 } }
}
#endif
namespace [[deprecated ("áæ")]] ND	// { dg-message "declared here" }
{
  int i;
};
int j = ND::i;				// { dg-warning "'ND' is deprecated: áæ" }
