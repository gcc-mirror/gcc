// P3176R1 - The Oxford variadic comma
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wdeprecated-variadic-comma-omission" }

void f1 (int...);		// { dg-warning "omission of ',' before varargs '...' is deprecated in" }
#if __cplusplus >= 202002L
void f2 (auto...);
void f3 (auto......);		// { dg-warning "omission of ',' before varargs '...' is deprecated in" "" { target c++20 } }
#endif
template <typename ...T>
void f4 (T......);		// { dg-warning "omission of ',' before varargs '...' is deprecated in" }
template <typename ...T>
void f5 (T...);
template <typename ...T>
void f6 (T..., int...);		// { dg-warning "omission of ',' before varargs '...' is deprecated in" }
void
f7 (char...)			// { dg-warning "omission of ',' before varargs '...' is deprecated in" }
{
}
