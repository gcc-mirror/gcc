// PR c++/117825
// { dg-do compile { target c++17 } }
// { dg-options "-Wformat -Wformat-security" }

__attribute__((format (printf, 1, 2)))
int fails (const char *, ...) { return 0; }

template <auto func, typename... Args>
auto wrap (Args... args) -> decltype (func (args...))
{
  return func (args...);	// { dg-warning "format not a string literal and no format arguments" }
}

int
main ()
{
  wrap<fails> ("Test!");
}
