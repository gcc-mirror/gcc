// { dg-options "-std=c++11" }

template< typename Fn > struct function;

template< typename Result, typename ... ArgTypes >
struct function< auto (ArgTypes...)->Result > {
};

int main()
{
   function< auto(double)->int > y;
   return 0;
}
