// PR c++/26433
// { dg-do link }

int get_int()
{
  throw 1;

  return 0;
}

template <class _T> class Test
{
public:
  Test()
        try
	: i(get_int())
  {
    i++;
  }
  catch(...)
  {
    // Syntax error caused by undefined __FUNCTION__.
    const char* ptr = __FUNCTION__;
  }

private:
  int i;
  _T t;
};

int main()
{
    try
      {
        Test<int> test;
      }
    catch(...)
      {
        return 1;
      }

    return 0;
}
