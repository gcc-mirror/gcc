/* { dg-options "-O2 -fdump-ipa-icf" } */
/* { dg-do run } */

int ctor_counter = 1;
int dtor_counter;

__attribute__((constructor))
void A()
{
  ctor_counter++;
}

__attribute__((destructor))
void B()
{
  if (dtor_counter == 0)
    __builtin_abort ();

  dtor_counter--;
}

__attribute__((constructor))
static void C() {
    ctor_counter++;
}

__attribute__((destructor))
static void D() {
  if (dtor_counter == 0)
    __builtin_abort ();

  dtor_counter--;
}

int main()
{
    if (ctor_counter != 3)
        __builtin_abort ();

    dtor_counter = 2;

    return 0;
}

/* { dg-final { scan-ipa-dump "Equal symbols: 0" "icf"  } } */
