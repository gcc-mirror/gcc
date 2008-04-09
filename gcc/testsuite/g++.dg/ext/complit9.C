// PR c++/35708
// { dg-options "" }

struct object { int one_o; int allocstamp; };
int pgci_pointable (object obj);
void foo(void);
int main (int argc, char *argv[])
{
  if (pgci_pointable((object){7,100}))
    {
      bad_rehash_size:
        foo();
    }
  goto bad_rehash_size;
}
