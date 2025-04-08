// { dg-do compile }
// { dg-options "-g" }

template <typename> class T
{
  typedef struct {} a __attribute__((aligned));
};
void f ()
{
  T<int>();
}
