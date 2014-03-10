// PR c++/40308, 40311
// { dg-do run { target c++11 } }

template< typename T >
struct test {
   test() : data{} {}

   T data;
};

int main()
{
   test<int> x;
   test<int*> y;
   int * a = new int{};
   int * b = new int{5};
   return 0;
}
