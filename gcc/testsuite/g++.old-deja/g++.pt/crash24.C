// { dg-do assemble  }

template<typename T, template <class> class U> void template_fn (T);
template<typename T> void callme ( void (*)(T));

template<typename T> struct S1;

int main()
{
  callme(&template_fn<double, S1>);
}
