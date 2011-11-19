// { dg-do compile }
// { dg-options "-fgnu-tm" }

class HashTree
{
public:
   __attribute__((transaction_safe))
   int add_element2();
private:
   int Count;
};


__attribute__((transaction_safe))
int HashTree::add_element2()
{
 int tt;
  __transaction_atomic {
    tt = Count;
 }
 return tt;
}
