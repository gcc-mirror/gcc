// Testcase for throw bad_array_new_length when new char[n].
// { dg-options -std=c++11 }
// { dg-do run }

#include <new>
#include <stdexcept>

struct s_char
{ 
  char ch;
};

struct s_char_with_new 
{
  char ch;
  void *operator new[] (std::size_t sz)
  {
    abort();
  }
};

void test_1()
{
  try 
  {
    int negative = -1;
    new char[negative];
  } 
  catch(const std::bad_array_new_length &e) 
  {
  }
}

void test_2()
{
  try 
  {
  	int negative = -1;
  	new s_char[negative];
  } 
  catch(const std::bad_array_new_length &e) 
  {
  }
}

void test_3()
{
  try 
  {
  	int negative = -1;
  	new s_char_with_new[negative];
  } 
  catch(const std::bad_array_new_length &e) 
  {
  }
}


int main()  
{
  test_1();
  test_2();
  test_3();
  return 0;
}
