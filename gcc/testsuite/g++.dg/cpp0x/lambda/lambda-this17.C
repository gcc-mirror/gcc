// PR c++/58481
// { dg-require-effective-target c++11 }

struct Test {
  template<typename... Args> inline void triggerTest (Args&&... fargs) { } 
};

struct TestPickled : Test {  
  template<typename... Args> void triggerTest (Args&&... fargs) { 
    [=](Args... as) { // { dg-warning "implicit capture" "" { target c++2a } }
      Test::triggerTest (as...);
    } ();		
  }
};

int main()
{
  TestPickled test;
  test.triggerTest ();
  return 0;
}
