// PR c++/17435
// { dg-do run }

extern "C" void abort ();

bool ok;
 
struct A  
{ 
  void func() const 
  { 
    ok = 1;
  } 
  
  ~A() 
  { 
    if (!ok)
      abort ();
  } 
}; 

struct B : public A  
{ 
}; 
 
int main() 
{ 
  A const& r1 = B(); 
  r1.func(); 
} 
