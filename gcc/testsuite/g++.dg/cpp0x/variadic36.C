// { dg-options "-std=gnu++0x" }
template<typename T, typename... Args>
void f(const T&, const Args&... args)
{
  f(args); // { dg-error "packs not expanded" }
}

template<typename... Values>
struct tuple_base { };

template<typename... Values>
struct tuple : tuple_base<Values> { }; // { dg-error "packs not expanded" }

// { dg-message "args" "note" { target *-*-* } 5 }
// { dg-message "Values" "note" { target *-*-* } 12 }
