/* { dg-do run } */
/* { dg-require-effective-target hwaddress_exec } */

/*
   Tests of nested funtions are:
    0) Accessing closed over variables works.
    1) Accesses outside of variables is caught.
    2) Accessing variable out of scope is caught.

    Here we test that accessing closed over variables works.
 */

/* We need a second layer of indirection so that GCC doesn't notice we're
   returning the address of a local variable and put 0 in it's place.  */
__attribute__((noinline))
int *Ident(void *x) {
  return x;
}

int __attribute__ ((noinline))
intermediate (void (*f) (int, char),
	      char num)
{
  if (num == 1)
    /* NOTE: We need to overrun by an amount greater than the "extra data" in a
       nonlocal goto structure.  The entire structure is allocated on the stack
       with a single tag, which means hwasan can't tell if a closed-over buffer
       was overrun by an amount small enough that the access was still to some
       data in that nonlocal goto structure.  */
    f (100, 100);
  else
    f (3, 100);
  /* Just return something ... */
  return num % 3;
}

int* __attribute__ ((noinline))
nested_function (char num)
{
  int big_array[16];
  int other_array[16];
  void store (int index, char value)
    { big_array[index] = value; }
  return Ident(&other_array[intermediate (store, num)]);
}

#ifndef MAIN
int main ()
{
  nested_function (0);
  return 0;
}
#endif
