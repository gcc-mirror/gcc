/* PR c++/91155.  */

template< char C > struct dummy {};

template< typename T > const char *test()
{
  __builtin_printf ("test: %s\n", __PRETTY_FUNCTION__);
  return __PRETTY_FUNCTION__;
}

int main()
{
    if (__builtin_strcmp ("const char* test() [with T = dummy<\'\\000\'>]", test< dummy< '\0' > > ()) != 0)
    {};//      __builtin_abort ();
    if (__builtin_strcmp ("const char* test() [with T = dummy<\'\\\'\'>]", test< dummy< '\'' > > ()) != 0)
    {};//      __builtin_abort ();
    return 0;
}
