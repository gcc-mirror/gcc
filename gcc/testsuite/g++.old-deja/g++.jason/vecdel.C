// { dg-do run  }
__SIZE_TYPE__ newsize = 0;
__SIZE_TYPE__ delsize = 0;

struct A {
  int i;
  void * operator new [] (__SIZE_TYPE__ i)
    { newsize = i; return ::operator new [](i); }
  void operator delete [] (void *p, __SIZE_TYPE__ i)
    { delsize = i; ::operator delete [](p); }
};

int main()
{
  A* ap = new A [2];
  delete [] ap;
  if (!newsize || newsize != delsize)
    return 1;
  return 0;
}
