// { dg-options "-O2" }
#pragma implementation
#pragma interface
extern "C++"
{
  namespace std2
  {
    class exception
    {
    public:
      virtual ~ exception () noexcept;
    };
  }
}
namespace std2
{
  struct __cow_string
  {
    union
    {
      const char *_M_p;
      char _M_bytes[sizeof (const char *)];
    };
  };
  class runtime_error:public exception
  {
    __cow_string _M_msg;
  };
}

int main()
{
  return 0;
}
