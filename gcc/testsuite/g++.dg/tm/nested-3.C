// { dg-do compile }
// { dg-options "-fgnu-tm -O0" }

// Same as nested-2.C but with no optimization.

typedef unsigned long int uint64_t;
extern int *hash_indx;

typedef struct
{
  uint64_t exit_atomicsec_time;
} ent_ex_times;
class HashTree
{
public:
   __attribute__((transaction_safe))
   void *operator new(__SIZE_TYPE__);
   __attribute__((transaction_safe))
   int add_element();
private:
   HashTree **Hash_table;
   int Count;
};


__attribute__((transaction_safe))
int HashTree::add_element()
{
 ent_ex_times enter_exit_times_inside;
 int val = hash_indx[5];
 int tt;
 if (Hash_table[val] == __null)
 {
  __transaction_atomic {
    Hash_table[val] = new HashTree;
  }
 }
 __transaction_atomic {
 tt = Count++;
 enter_exit_times_inside.exit_atomicsec_time = 5;
 }
 return tt;
}
