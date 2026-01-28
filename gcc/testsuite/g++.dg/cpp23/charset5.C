// P2246R1
// { dg-do compile { target c++11 } }
// { dg-require-iconv "UTF-8" }
// { dg-options "-fexec-charset=UTF-8" }

[[gnu::deprecated ("áæ")]] int d;	// { dg-message "declared here" }
int e = d;				// { dg-warning "'d' is deprecated: áæ" }
[[gnu::unavailable ("áæ")]] int f;	// { dg-message "declared here" }
int g = f;				// { dg-error "'f' is unavailable: áæ" }
__attribute__((deprecated ("áæ"))) int h; // { dg-message "declared here" }
int i = h;				// { dg-warning "'h' is deprecated: áæ" }
__attribute__((unavailable ("áæ"))) int j;	// { dg-message "declared here" }
int k = j;				// { dg-error "'j' is unavailable: áæ" }
#warning "áæ"				// { dg-warning "#warning \"áæ\"" }
namespace [[gnu::deprecated ("áæ")]] ND // { dg-message "declared here" }
{
  int l;
};
int m = ND::l;				// { dg-warning "'ND' is deprecated: áæ" }
namespace __attribute__((deprecated ("áæ"))) NE // { dg-message "declared here" }
{
  int l;
};
int n = NE::l;				// { dg-warning "'NE' is deprecated: áæ" }
