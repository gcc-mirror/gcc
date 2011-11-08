// { dg-do compile }
// { dg-options "-fgnu-tm -O3" }

class HashTree
{
   __attribute__((transaction_safe)) void rehash();
   HashTree **Hash_table;
   int Hash_function;
};

__attribute__((transaction_safe)) void HashTree::rehash()
{
   for (int i=0; i < Hash_function; i++)
      Hash_table[i] = 0;
}
