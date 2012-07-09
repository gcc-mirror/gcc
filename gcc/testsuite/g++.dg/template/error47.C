// PR c++/51367

template<typename T> void foo(T, T); // { dg-message "template" }

void bar(void* p)
{
  foo(0, p); // { dg-error "no matching" }
}
// { dg-message "candidate|parameter 'T' ('int' and 'void*')" "" { target *-*-* } 7 }
