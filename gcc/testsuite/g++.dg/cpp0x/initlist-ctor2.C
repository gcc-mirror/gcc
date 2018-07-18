// PR c++/83937
// { dg-do run { target c++11 } }

struct S
{
  S(int v = 42) {
    if (v != int{})
      __builtin_abort();
  }
};

int main()
{
  S( {} );
} 
