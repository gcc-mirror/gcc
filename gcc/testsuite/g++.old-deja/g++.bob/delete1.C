// { dg-do run  }
// prms-id: 7330
#include <stddef.h>
int size = 0; 

struct X {
    int x;
    void *operator new[](size_t sz) throw()  {
         size = sz;
         return 0;  
    }
    void operator delete[] (void *vp) { ::operator delete(vp); }  
};
int main()
{
     X (*px) [10];

     px = new X[5][10];

     delete [] px;

     return 0;
}
