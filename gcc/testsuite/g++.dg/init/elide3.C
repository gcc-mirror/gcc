// PR c++/67557
// { dg-do run }

namespace std
{
  struct string
  {
    typedef unsigned long size_type;
    const char* _M_p;
    char        _M_local_buf[1];

    string(const char* s) : _M_p(_M_local_buf)
    {
      __builtin_printf("%p constructed\n", this);
    }

    string(const string& s) : _M_p(_M_local_buf)
    {
      __builtin_printf("%p copied from %p\n", this, &s);
    }

    ~string()
    {
      __builtin_printf("%p destroyed\n", this);
      if (_M_p != _M_local_buf)
	__builtin_abort();
    }
  };
}

struct StartTag
{
  explicit StartTag(std::string const & tag) : tag_(tag), keepempty_(false) {}
  std::string tag_;
  bool keepempty_;
};

StartTag fontToStartTag() { return StartTag(""); }

struct FontTag : public StartTag
{
  FontTag() : StartTag(fontToStartTag()) {}
};

int main()
{
  FontTag x;
  __builtin_printf("%p x.tag_ in main()\n", &x.tag_);
  return 0;
}
