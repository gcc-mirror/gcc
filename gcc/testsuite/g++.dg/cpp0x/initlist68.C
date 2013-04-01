// PR c++/56772
// { dg-require-effective-target c++11 }

typedef __SIZE_TYPE__ size_t;
void* operator new[](size_t, void *p) { return p; }
template <typename T = size_t>
void f ()
{
  size_t coord [2][2];
  new (&coord) size_t [2][2]
   {
     {0,0},
     {0,0},
   };
}

int main ()
{
   f<>();
}
