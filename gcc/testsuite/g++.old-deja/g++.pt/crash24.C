// Build don't link:

template<typename T, template <class> class U> void template_fn (T);
template<typename T, typename U> void callme ( void (*)(T));

template<typename T> struct S1;

int main()
{
  callme( template_fn<double, S1>); // ERROR - no matching function
}
