/* PR middle-end/95886 - suboptimal memcpy with embedded zero bytes
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-rtl-expand" } */

const char a1234567890[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };

void cpy_123456789 (void *d)
{
  /* Expands into:
       movabsq  $578437695752307201, %rax
       movb     $9, 8(%rdi)
       movq     %rax, (%rdi)  */
  __builtin_memcpy (d, a1234567890, 9);
}

const char a1234567800[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 0 };

void cpy_1234567800 (void *d)
{
  /* Expands into:
       movabsq  $578437695752307201, %rax
       movb     $0, 8(%rdi)
       movq     %rax, (%rdi)  */
  __builtin_memcpy (d, a1234567800, 9);
}

/* { dg-final { scan-rtl-dump-times "const_int 578437695752307201" 2 "expand"} } */


const char a0234567890[10] = { 0, 2, 3, 4, 5, 6, 7, 8, 9 };

void cpy_023456789 (void *d)
{
  __builtin_memcpy (d, a0234567890, 9);
}

/* { dg-final { scan-rtl-dump-times "const_int 578437695752307200" 1 "expand"} } */


const char a1034567890[10] = { 1, 0, 3, 4, 5, 6, 7, 8, 9 };

void cpy_103456789 (void *d)
{
  __builtin_memcpy (d, a1034567890, 9);
}

/* { dg-final { scan-rtl-dump-times "const_int 578437695752306689" 1 "expand"} } */


const char a1204567890[10] = { 1, 2, 0, 4, 5, 6, 7, 8, 9 };

void cpy_120456789 (void *d)
{
  __builtin_memcpy (d, a1204567890, 9);
}

/* { dg-final { scan-rtl-dump-times "const_int 578437695752110593" 1 "expand"} } */


const char a1230567890[10] = { 1, 2, 3, 0, 5, 6, 7, 8, 9 };

void cpy_123056789 (void *d)
{
  __builtin_memcpy (d, a1230567890, 9);
}

/* { dg-final { scan-rtl-dump-times "const_int 578437695685198337" 1 "expand"} } */


const char a1234067890[10] = { 1, 2, 3, 4, 0, 6, 7, 8, 9 };

void cpy_123406789 (void *d)
{
  __builtin_memcpy (d, a1234067890, 9);
}

/* { dg-final { scan-rtl-dump-times "const_int 578437695685198337" 1 "expand"} } */


const char a1234507890[10] = { 1, 2, 3, 4, 5, 0, 7, 8, 9 };

void cpy_123450789 (void *d)
{
  __builtin_memcpy (d, a1234507890, 9);
}

/* { dg-final { scan-rtl-dump-times "const_int 578431098682540545" 1 "expand"} } */


const char a1234560890[10] = { 1, 2, 3, 4, 5, 6, 0, 8, 9 };

void cpy_123456089 (void *d)
{
  __builtin_memcpy (d, a1234560890, 9);
}

/* { dg-final { scan-rtl-dump-times "const_int 576467370915332609" 1 "expand"} } */


const char a1234567090[10] = { 1, 2, 3, 4, 5, 6, 7, 0, 9 };

void cpy_123456709 (void *d)
{
  __builtin_memcpy (d, a1234567090, 9);
}

/* { dg-final { scan-rtl-dump-times "const_int 1976943448883713" 1 "expand"} } */
