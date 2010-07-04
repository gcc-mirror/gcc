typedef float mm128 __attribute ((vector_size (16)));

template <class T>
struct A
{
  static T t;
};

void f (mm128 *);
