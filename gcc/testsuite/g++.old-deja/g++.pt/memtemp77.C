extern "C" int strcmp(const char*, const char*);

template <class T>
struct S3
{
  template <class U>
  static char* h(U);
};

template <>
template <>
char* S3<double>::h(int) { return __PRETTY_FUNCTION__; }

template <>
template <>
char* S3<char>::h(int) { return __PRETTY_FUNCTION__; }

int main()
{
  if (strcmp (S3<double>::h(7), 
	      "static char * S3<double>::h<int>(int)") == 0)
    return 0;
  else 
    return 1;
}
