// Testcase for handling of typedef wierdness.
// Build don't link:

template <class T>
struct A
{
  typedef enum
  {
    foo
  } B;

  A (B b);
};

template <class T>
A<T>::A (B b)
{
}
