//Lifetime of temporaries: 
//egcs 2.92 performs cleanup for temporaries inside new expressions
//after the new is complete, not at the end of the full expression.

#include <new>
#include <cstdlib>
#include <cstdio>

bool new_throws;
bool ctor_throws;

int new_done;
int ctor_done;
int func_done;
int dtor_done;
int delete_done;

int count;

void init()
{
  new_throws = ctor_throws = false;
  new_done = ctor_done = func_done = dtor_done = delete_done = count = 0;
}

struct line_error{
  int line;
  line_error(int i):line(i){}
};

#define CHECK(cond)  if(!(cond))throw std::line_error(__LINE__);

struct A{
  A(int){
    ctor_done = ++count;
    if(ctor_throws)
      throw 1;
  }
  A(const A&){
    CHECK(false); //no copy constructors in this code
  }
  ~A(){
    dtor_done = ++count;
  }
  A* addr(){return this;}
};

struct B{
  B(A*){}
  void* operator new(size_t s){
    new_done = ++count;
    if(new_throws)
      throw 1;
    return malloc(s);
  }
  void operator delete(void *){
    delete_done = ++count;
  }
};

void func(B* )
{
  func_done = ++count;
}

void test1()
{
  init();
  try{
    func(new B(A(10).addr()));
  }catch(int){
  }
  CHECK(new_done==1);
  CHECK(ctor_done==2);
  CHECK(func_done==3);
  CHECK(dtor_done==4);
  CHECK(delete_done==0);
}

void test2()
{
  init();
  new_throws = true;
  try{
    func(new B(A(10).addr()));
  }catch(int){
  }
  CHECK(new_done==1);
  CHECK(ctor_done==0);
  CHECK(func_done==0);
  CHECK(dtor_done==0);
  CHECK(delete_done==0);
}

void test3()
{
  init();
  ctor_throws = true;
  try{
    func(new B(A(10).addr()));
  }catch(int){
  }
  CHECK(new_done==1);
  CHECK(ctor_done==2);
  CHECK(func_done==0);
  CHECK(dtor_done==0);
  CHECK(delete_done==3);
}

int main()
{
  try{
    test1();
    test2();
    test3();
  }catch(line_error e){
    printf("Got error in line %d\n",e.line);
    return 1;
  }
  return 0;
}
