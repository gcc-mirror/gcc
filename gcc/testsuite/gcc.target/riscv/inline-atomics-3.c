/* Check all char alignments.  */
/* Duplicate logic as libatomic/testsuite/libatomic.c/atomic-op-1.c */
/* Test __atomic routines for existence and proper execution on 1 byte
   values with each valid memory model.  */
/* { dg-do run { target { riscv_a } } } */
/* { dg-options "-minline-atomics -Wno-address-of-packed-member" } */

/* Test the execution of the __atomic_*OP builtin routines for a char.  */

extern void abort(void);

char count, res;
const char init = ~0;

struct A
{
   char a;
   char b;
   char c;
   char d;
} __attribute__ ((packed)) A;

/* The fetch_op routines return the original value before the operation.  */

void
test_fetch_add (char* v)
{
  *v = 0;
  count = 1;

  if (__atomic_fetch_add (v, count, __ATOMIC_RELAXED) != 0)
    abort ();

  if (__atomic_fetch_add (v, 1, __ATOMIC_CONSUME) != 1)
    abort ();

  if (__atomic_fetch_add (v, count, __ATOMIC_ACQUIRE) != 2)
    abort ();

  if (__atomic_fetch_add (v, 1, __ATOMIC_RELEASE) != 3)
    abort ();

  if (__atomic_fetch_add (v, count, __ATOMIC_ACQ_REL) != 4)
    abort ();

  if (__atomic_fetch_add (v, 1, __ATOMIC_SEQ_CST) != 5)
    abort ();
}


void
test_fetch_sub (char* v)
{
  *v = res = 20;
  count = 0;

  if (__atomic_fetch_sub (v, count + 1, __ATOMIC_RELAXED) !=  res--)
    abort ();

  if (__atomic_fetch_sub (v, 1, __ATOMIC_CONSUME) !=  res--)
    abort ();

  if (__atomic_fetch_sub (v, count + 1, __ATOMIC_ACQUIRE) !=  res--)
    abort ();

  if (__atomic_fetch_sub (v, 1, __ATOMIC_RELEASE) !=  res--)
    abort ();

  if (__atomic_fetch_sub (v, count + 1, __ATOMIC_ACQ_REL) !=  res--)
    abort ();

  if (__atomic_fetch_sub (v, 1, __ATOMIC_SEQ_CST) !=  res--)
    abort ();
}

void
test_fetch_and (char* v)
{
  *v = init;

  if (__atomic_fetch_and (v, 0, __ATOMIC_RELAXED) !=  init)
    abort ();

  if (__atomic_fetch_and (v, init, __ATOMIC_CONSUME) !=  0)
    abort ();

  if (__atomic_fetch_and (v, 0, __ATOMIC_ACQUIRE) !=  0)
    abort ();

  *v = ~*v;
  if (__atomic_fetch_and (v, init, __ATOMIC_RELEASE) !=  init)
    abort ();

  if (__atomic_fetch_and (v, 0, __ATOMIC_ACQ_REL) !=  init)
    abort ();

  if (__atomic_fetch_and (v, 0, __ATOMIC_SEQ_CST) !=  0)
    abort ();
}

void
test_fetch_nand (char* v)
{
  *v = init;

  if (__atomic_fetch_nand (v, 0, __ATOMIC_RELAXED) !=  init)
    abort ();

  if (__atomic_fetch_nand (v, init, __ATOMIC_CONSUME) !=  init)
    abort ();

  if (__atomic_fetch_nand (v, 0, __ATOMIC_ACQUIRE) !=  0 )
    abort ();

  if (__atomic_fetch_nand (v, init, __ATOMIC_RELEASE) !=  init)
    abort ();

  if (__atomic_fetch_nand (v, init, __ATOMIC_ACQ_REL) !=  0)
    abort ();

  if (__atomic_fetch_nand (v, 0, __ATOMIC_SEQ_CST) !=  init)
    abort ();
}

void
test_fetch_xor (char* v)
{
  *v = init;
  count = 0;

  if (__atomic_fetch_xor (v, count, __ATOMIC_RELAXED) !=  init)
    abort ();

  if (__atomic_fetch_xor (v, ~count, __ATOMIC_CONSUME) !=  init)
    abort ();

  if (__atomic_fetch_xor (v, 0, __ATOMIC_ACQUIRE) !=  0)
    abort ();

  if (__atomic_fetch_xor (v, ~count, __ATOMIC_RELEASE) !=  0)
    abort ();

  if (__atomic_fetch_xor (v, 0, __ATOMIC_ACQ_REL) !=  init)
    abort ();

  if (__atomic_fetch_xor (v, ~count, __ATOMIC_SEQ_CST) !=  init)
    abort ();
}

void
test_fetch_or (char* v)
{
  *v = 0;
  count = 1;

  if (__atomic_fetch_or (v, count, __ATOMIC_RELAXED) !=  0)
    abort ();

  count *= 2;
  if (__atomic_fetch_or (v, 2, __ATOMIC_CONSUME) !=  1)
    abort ();

  count *= 2;
  if (__atomic_fetch_or (v, count, __ATOMIC_ACQUIRE) !=  3)
    abort ();

  count *= 2;
  if (__atomic_fetch_or (v, 8, __ATOMIC_RELEASE) !=  7)
    abort ();

  count *= 2;
  if (__atomic_fetch_or (v, count, __ATOMIC_ACQ_REL) !=  15)
    abort ();

  count *= 2;
  if (__atomic_fetch_or (v, count, __ATOMIC_SEQ_CST) !=  31)
    abort ();
}

/* The OP_fetch routines return the new value after the operation.  */

void
test_add_fetch (char* v)
{
  *v = 0;
  count = 1;

  if (__atomic_add_fetch (v, count, __ATOMIC_RELAXED) != 1)
    abort ();

  if (__atomic_add_fetch (v, 1, __ATOMIC_CONSUME) != 2)
    abort ();

  if (__atomic_add_fetch (v, count, __ATOMIC_ACQUIRE) != 3)
    abort ();

  if (__atomic_add_fetch (v, 1, __ATOMIC_RELEASE) != 4)
    abort ();

  if (__atomic_add_fetch (v, count, __ATOMIC_ACQ_REL) != 5)
    abort ();

  if (__atomic_add_fetch (v, count, __ATOMIC_SEQ_CST) != 6)
    abort ();
}


void
test_sub_fetch (char* v)
{
  *v = res = 20;
  count = 0;

  if (__atomic_sub_fetch (v, count + 1, __ATOMIC_RELAXED) !=  --res)
    abort ();

  if (__atomic_sub_fetch (v, 1, __ATOMIC_CONSUME) !=  --res)
    abort ();

  if (__atomic_sub_fetch (v, count + 1, __ATOMIC_ACQUIRE) !=  --res)
    abort ();

  if (__atomic_sub_fetch (v, 1, __ATOMIC_RELEASE) !=  --res)
    abort ();

  if (__atomic_sub_fetch (v, count + 1, __ATOMIC_ACQ_REL) !=  --res)
    abort ();

  if (__atomic_sub_fetch (v, count + 1, __ATOMIC_SEQ_CST) !=  --res)
    abort ();
}

void
test_and_fetch (char* v)
{
  *v = init;

  if (__atomic_and_fetch (v, 0, __ATOMIC_RELAXED) !=  0)
    abort ();

  *v = init;
  if (__atomic_and_fetch (v, init, __ATOMIC_CONSUME) !=  init)
    abort ();

  if (__atomic_and_fetch (v, 0, __ATOMIC_ACQUIRE) !=  0)
    abort ();

  *v = ~*v;
  if (__atomic_and_fetch (v, init, __ATOMIC_RELEASE) !=  init)
    abort ();

  if (__atomic_and_fetch (v, 0, __ATOMIC_ACQ_REL) !=  0)
    abort ();

  *v = ~*v;
  if (__atomic_and_fetch (v, 0, __ATOMIC_SEQ_CST) !=  0)
    abort ();
}

void
test_nand_fetch (char* v)
{
  *v = init;

  if (__atomic_nand_fetch (v, 0, __ATOMIC_RELAXED) !=  init)
    abort ();

  if (__atomic_nand_fetch (v, init, __ATOMIC_CONSUME) !=  0)
    abort ();

  if (__atomic_nand_fetch (v, 0, __ATOMIC_ACQUIRE) !=  init)
    abort ();

  if (__atomic_nand_fetch (v, init, __ATOMIC_RELEASE) !=  0)
    abort ();

  if (__atomic_nand_fetch (v, init, __ATOMIC_ACQ_REL) !=  init)
    abort ();

  if (__atomic_nand_fetch (v, 0, __ATOMIC_SEQ_CST) !=  init)
    abort ();
}



void
test_xor_fetch (char* v)
{
  *v = init;
  count = 0;

  if (__atomic_xor_fetch (v, count, __ATOMIC_RELAXED) !=  init)
    abort ();

  if (__atomic_xor_fetch (v, ~count, __ATOMIC_CONSUME) !=  0)
    abort ();

  if (__atomic_xor_fetch (v, 0, __ATOMIC_ACQUIRE) !=  0)
    abort ();

  if (__atomic_xor_fetch (v, ~count, __ATOMIC_RELEASE) !=  init)
    abort ();

  if (__atomic_xor_fetch (v, 0, __ATOMIC_ACQ_REL) !=  init)
    abort ();

  if (__atomic_xor_fetch (v, ~count, __ATOMIC_SEQ_CST) !=  0)
    abort ();
}

void
test_or_fetch (char* v)
{
  *v = 0;
  count = 1;

  if (__atomic_or_fetch (v, count, __ATOMIC_RELAXED) !=  1)
    abort ();

  count *= 2;
  if (__atomic_or_fetch (v, 2, __ATOMIC_CONSUME) !=  3)
    abort ();

  count *= 2;
  if (__atomic_or_fetch (v, count, __ATOMIC_ACQUIRE) !=  7)
    abort ();

  count *= 2;
  if (__atomic_or_fetch (v, 8, __ATOMIC_RELEASE) !=  15)
    abort ();

  count *= 2;
  if (__atomic_or_fetch (v, count, __ATOMIC_ACQ_REL) !=  31)
    abort ();

  count *= 2;
  if (__atomic_or_fetch (v, count, __ATOMIC_SEQ_CST) !=  63)
    abort ();
}


/* Test the OP routines with a result which isn't used. Use both variations
   within each function.  */

void
test_add (char* v)
{
  *v = 0;
  count = 1;

  __atomic_add_fetch (v, count, __ATOMIC_RELAXED);
  if (*v != 1)
    abort ();

  __atomic_fetch_add (v, count, __ATOMIC_CONSUME);
  if (*v != 2)
    abort ();

  __atomic_add_fetch (v, 1 , __ATOMIC_ACQUIRE);
  if (*v != 3)
    abort ();

  __atomic_fetch_add (v, 1, __ATOMIC_RELEASE);
  if (*v != 4)
    abort ();

  __atomic_add_fetch (v, count, __ATOMIC_ACQ_REL);
  if (*v != 5)
    abort ();

  __atomic_fetch_add (v, count, __ATOMIC_SEQ_CST);
  if (*v != 6)
    abort ();
}


void
test_sub (char* v)
{
  *v = res = 20;
  count = 0;

  __atomic_sub_fetch (v, count + 1, __ATOMIC_RELAXED);
  if (*v != --res)
    abort ();

  __atomic_fetch_sub (v, count + 1, __ATOMIC_CONSUME);
  if (*v != --res)
    abort ();

  __atomic_sub_fetch (v, 1, __ATOMIC_ACQUIRE);
  if (*v != --res)
    abort ();

  __atomic_fetch_sub (v, 1, __ATOMIC_RELEASE);
  if (*v != --res)
    abort ();

  __atomic_sub_fetch (v, count + 1, __ATOMIC_ACQ_REL);
  if (*v != --res)
    abort ();

  __atomic_fetch_sub (v, count + 1, __ATOMIC_SEQ_CST);
  if (*v != --res)
    abort ();
}

void
test_and (char* v)
{
  *v = init;

  __atomic_and_fetch (v, 0, __ATOMIC_RELAXED);
  if (*v != 0)
    abort ();

  *v = init;
  __atomic_fetch_and (v, init, __ATOMIC_CONSUME);
  if (*v != init)
    abort ();

  __atomic_and_fetch (v, 0, __ATOMIC_ACQUIRE);
  if (*v != 0)
    abort ();

  *v = ~*v;
  __atomic_fetch_and (v, init, __ATOMIC_RELEASE);
  if (*v != init)
    abort ();

  __atomic_and_fetch (v, 0, __ATOMIC_ACQ_REL);
  if (*v != 0)
    abort ();

  *v = ~*v;
  __atomic_fetch_and (v, 0, __ATOMIC_SEQ_CST);
  if (*v != 0)
    abort ();
}

void
test_nand (char* v)
{
  *v = init;

  __atomic_fetch_nand (v, 0, __ATOMIC_RELAXED);
  if (*v != init)
    abort ();

  __atomic_fetch_nand (v, init, __ATOMIC_CONSUME);
  if (*v != 0)
    abort ();

  __atomic_nand_fetch (v, 0, __ATOMIC_ACQUIRE);
  if (*v != init)
    abort ();

  __atomic_nand_fetch (v, init, __ATOMIC_RELEASE);
  if (*v != 0)
    abort ();

  __atomic_fetch_nand (v, init, __ATOMIC_ACQ_REL);
  if (*v != init)
    abort ();

  __atomic_nand_fetch (v, 0, __ATOMIC_SEQ_CST);
  if (*v != init)
    abort ();
}



void
test_xor (char* v)
{
  *v = init;
  count = 0;

  __atomic_xor_fetch (v, count, __ATOMIC_RELAXED);
  if (*v != init)
    abort ();

  __atomic_fetch_xor (v, ~count, __ATOMIC_CONSUME);
  if (*v != 0)
    abort ();

  __atomic_xor_fetch (v, 0, __ATOMIC_ACQUIRE);
  if (*v != 0)
    abort ();

  __atomic_fetch_xor (v, ~count, __ATOMIC_RELEASE);
  if (*v != init)
    abort ();

  __atomic_fetch_xor (v, 0, __ATOMIC_ACQ_REL);
  if (*v != init)
    abort ();

  __atomic_xor_fetch (v, ~count, __ATOMIC_SEQ_CST);
  if (*v != 0)
    abort ();
}

void
test_or (char* v)
{
  *v = 0;
  count = 1;

  __atomic_or_fetch (v, count, __ATOMIC_RELAXED);
  if (*v != 1)
    abort ();

  count *= 2;
  __atomic_fetch_or (v, count, __ATOMIC_CONSUME);
  if (*v != 3)
    abort ();

  count *= 2;
  __atomic_or_fetch (v, 4, __ATOMIC_ACQUIRE);
  if (*v != 7)
    abort ();

  count *= 2;
  __atomic_fetch_or (v, 8, __ATOMIC_RELEASE);
  if (*v != 15)
    abort ();

  count *= 2;
  __atomic_or_fetch (v, count, __ATOMIC_ACQ_REL);
  if (*v != 31)
    abort ();

  count *= 2;
  __atomic_fetch_or (v, count, __ATOMIC_SEQ_CST);
  if (*v != 63)
    abort ();
}

int
main ()
{
  char* V[] = {&A.a, &A.b, &A.c, &A.d};

  for (int i = 0; i < 4; i++) {
    test_fetch_add (V[i]);
    test_fetch_sub (V[i]);
    test_fetch_and (V[i]);
    test_fetch_nand (V[i]);
    test_fetch_xor (V[i]);
    test_fetch_or (V[i]);

    test_add_fetch (V[i]);
    test_sub_fetch (V[i]);
    test_and_fetch (V[i]);
    test_nand_fetch (V[i]);
    test_xor_fetch (V[i]);
    test_or_fetch (V[i]);

    test_add (V[i]);
    test_sub (V[i]);
    test_and (V[i]);
    test_nand (V[i]);
    test_xor (V[i]);
    test_or (V[i]);
  }

  return 0;
}
