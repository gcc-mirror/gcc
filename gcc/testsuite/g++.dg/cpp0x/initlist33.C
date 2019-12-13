// PR c++/44045
// { dg-do compile { target c++11 } }

struct base
{
   virtual ~base() { }
};

int main()
{
 base ptr_array[1];
 ptr_array = { base() };	// { dg-error "12:assigning to an array from an initializer list" }
}
