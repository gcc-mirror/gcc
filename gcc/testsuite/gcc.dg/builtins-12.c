/* Copyright (C) 2003  Free Software Foundation.

   Verify that all the malloc-like __builtin_ allocation functions are
   recognized by the compiler.

   Written by Roger Sayle, 12th April 2003.  */

/* { dg-do compile } */
/* { dg-options "-ansi" } */
/* { dg-final { scan-assembler-not "__builtin_" } } */

typedef __SIZE_TYPE__ size_t;

void *test1(size_t n)
{
  return __builtin_malloc(n);
}

void *test2(size_t n, size_t s)
{
  return __builtin_calloc(n,s);
}

char *test3(const char *ptr)
{
  return __builtin_strdup(ptr);
}

