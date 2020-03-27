// PR c++/94186
// { dg-do compile { target concepts } }

template <typename T>
struct is_small
{
  enum { value = sizeof(T) <= 4 };
};

template <typename T>
  requires is_small<T>::value	// { dg-error "bool" }
void fun(T) {}

template <typename T>
void fun(T) {}

int main()
{
  fun(1);  // { dg-message "" }
}
