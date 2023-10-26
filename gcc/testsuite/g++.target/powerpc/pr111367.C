/* { dg-do assemble } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -fstack-protector-strong" } */

/* Verify object file can be generated successfully.  */

struct SortAscending
{
};

typedef unsigned long long size_t;

void VQSort (long long *, size_t, SortAscending);

void
BenchAllColdSort ()
{
  typedef long long T;
  constexpr size_t kSize = 10 * 1000;
  alignas (16) T items[kSize];
  VQSort (items, kSize, SortAscending ());
}
