// { dg-do compile { target c++11 } }
// { dg-options "-pedantic" }

const char* foo()
{
  return __func__;
}
