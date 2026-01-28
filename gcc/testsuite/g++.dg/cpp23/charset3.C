// P2246R1
// { dg-do compile { target c++11 } }
// { dg-require-iconv "IBM1047" }
// { dg-options "-fexec-charset=IBM1047" }

[[gnu::deprecated ("foo")]] int d;	// { dg-message "declared here" }
int e = d;				// { dg-warning "'d' is deprecated: foo" }
[[gnu::unavailable ("bar")]] int f;	// { dg-message "declared here" }
int g = f;				// { dg-error "'f' is unavailable: bar" }
__attribute__((deprecated ("baz"))) int h; // { dg-message "declared here" }
int i = h;				// { dg-warning "'h' is deprecated: baz" }
__attribute__((unavailable ("qux"))) int j;	// { dg-message "declared here" }
int k = j;				// { dg-error "'j' is unavailable: qux" }
#warning "fred"				// { dg-warning "#warning \"fred\"" }
namespace [[gnu::deprecated ("corge")]] ND // { dg-message "declared here" }
{
  int l;
};
int m = ND::l;				// { dg-warning "'ND' is deprecated: corge" }
namespace __attribute__((deprecated ("xyzzy"))) NE // { dg-message "declared here" }
{
  int l;
};
int n = NE::l;				// { dg-warning "'NE' is deprecated: xyzzy" }
