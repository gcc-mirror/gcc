// g++ crashed because we unsaved the TARGET_EXPR for the return value
// for get_allocator without first expanding it, because it was part of the
// cleanup for the temporary string.

// Derived from libstdc++ v3 code.

// Special g++ Options: -O2
// Build don't link:

class AA {};

void fee (const AA&);

class basic_string
{
public:
  basic_string(const char*);

  ~basic_string() 
  { fee (this->get_allocator()); }

  AA get_allocator();
};

class failure
{
public:
  failure(const basic_string& __str);
};

class foo
{
public:
  foo(int x)
  {
    throw failure ("");
  }
};

void test05()
{
  foo ofs(0);
}
