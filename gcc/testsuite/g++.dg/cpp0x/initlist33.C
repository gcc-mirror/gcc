// PR c++/44045
// { dg-options "-std=c++0x" }

struct base
{
   virtual ~base() { }
};

int main()
{
 base ptr_array[1];
 ptr_array = { base() };	// { dg-error "assign" }
}
