// { dg-do run  }
extern "C" int strcmp(const char*, const char*);

template <class T>
struct S3
{
  template <class U>
  static const char* h(U);
};

template <>
template <>
const char* S3<double>::h(int) { return __PRETTY_FUNCTION__; }

template <>
template <>
const char* S3<char>::h(int) { return __PRETTY_FUNCTION__; }

int main()
{
  if (strcmp (S3<double>::h(7), 
	      "static const char* S3<T>::h(U) [with U = int, T = double]") == 0)
    return 0;
  else 
    return 1;
}
