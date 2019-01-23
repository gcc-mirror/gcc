// PR c++/88741

template <class T>
void foo()
{
  char row[] = {"test"};
}
  
void bar()
{
  foo<int>();
} 
