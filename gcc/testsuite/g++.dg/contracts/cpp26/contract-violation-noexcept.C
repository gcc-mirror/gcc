// test that the default contract violation handler can't throw
// { dg-do compile { target c++23 } }
// { dg-additional-options "-fcontracts -fcontract-evaluation-semantic=observe " }

#include <iostream>
#include <exception>
#include <cstdlib>

// Throws on all overflow and underflow calls.
struct underflow_error: std::exception { };
struct overflow_error: std::exception { };
struct positioning_error: std::exception { };

template<typename T>
struct fail_buf
  : public T
{
  typedef typename T::char_type   char_type;
  typedef typename T::int_type    int_type;
  typedef typename T::off_type    off_type;
  typedef typename T::pos_type    pos_type;

private:
  char_type p[2];

public:
  fail_buf()
  {
    p[0] = char_type('s');
    p[1] = char_type();
    this->setg(p, p, p + 1);
  }

  virtual int_type underflow()
  {
    throw underflow_error();
    return int_type();
  }

  virtual int_type uflow()
  {
    throw underflow_error();
    return int_type();
  }

  virtual int_type
  overflow(int_type)
  {
    throw overflow_error();
    return int_type();
  }

  virtual pos_type
  seekoff(off_type, std::ios_base::seekdir, std::ios_base::openmode)
  {
    throw positioning_error();
    return pos_type(off_type(-1));
  }

  virtual pos_type
  seekpos(pos_type, std::ios_base::openmode)
  {
    throw positioning_error();
    return pos_type(off_type(-1));
  }

  virtual int
  sync()
  {
    throw positioning_error();
    return 0;
  }
};

typedef  fail_buf<std::streambuf>   fail_streambuf;
std::streambuf* g_buf_to_restore;

// Test that there is an active exception when we reach the terminate handler.
void my_term()
{
  std::cerr.rdbuf(g_buf_to_restore); // be nice, restore the buf of a global stream
  try { throw; }
  catch(const underflow_error&) { std::exit(0); }
  catch(const overflow_error&) { std::exit(0); }
  catch(const positioning_error&) { std::exit(0); }
}



void f(int x) pre(x >= 0)
{
  try{
   int i = 1;
  }
  catch(...) {
  }
}

int main()
{
  std::set_terminate (my_term);
  fail_streambuf buf;
  g_buf_to_restore = std::cerr.rdbuf(&buf);
  std::cerr.exceptions(std::ios::badbit | std::ios::failbit | std::ios::eofbit);
  try
  {
      f(-42);
  } catch (...) {
  }
  // We should not get here
  return 1;
}
