/* Verify the handling of anti-ranges/multi-ranges by allocation functions
   and subsequent accesses.
   { dg-do compile }
   { dg-options "-O2" } */

typedef __SIZE_TYPE__ size_t;

void* malloc (size_t);
void  bzero (void*, size_t);
void* memset (void*, int, size_t);


/* Exercise size_t (via malloc and memset) and unsigned/signed int.  */

__attribute__ ((alloc_size (1))) void*
alloc_int (int);

__attribute__ ((access (write_only, 1, 2))) void
access_int (void*, int);

__attribute__ ((alloc_size (1))) void*
alloc_uint (unsigned);

__attribute__ ((access (write_only, 1, 2))) void
access_uint (void*, unsigned);


void* nowarn_malloc_memset_same_anti_range (size_t n)
{
  /* Set N to the anti-range ~[3, 3].  */
  if (n == 3)
    n = 4;
  void *p = malloc (n);

  /* Verify there is no warning for an access to N bytes at P.
     This means the warning has to assume the value of N in the call
     to alloc() is in the larger subrange [4, UINT_MAX], while in
     the call to access() in [0, 3].  */
  return memset (p, 0, n);
}

/* Same as above but with two valid ranges.  */

void* nowarn_malloc_memset_anti_range (size_t n1, size_t n2)
{
  /* Set N1 to the anti-range ~[3, 3].  */
  if (n1 == 3)
    n1 = 4;
  void *p = malloc (n1);

  /* Set N2 to the anti-range ~[7, 7].  */
  if (n2 == 7)
    n2 = 8;

  return memset (p, 0, n2);
}


void nowarn_alloc_access_same_anti_range_int (int n)
{
  /* Set N to the anti-range ~[3, 3].  */
  if (n == 3)
    n = 4;
  void *p = alloc_int (n);

  /* Verify there is no warning for an access to N bytes at P.
     This means the warning has to assume the value of N in the call
     to alloc() is in the larger subrange [4, UINT_MAX], while in
     the call to access() in [0, 3].  */
  access_int (p, n);
}

/* Same as above but with two valid ranges.  */

void nowarn_alloc_access_anti_range_int (int n1, int n2)
{
  /* Set N1 to the anti-range ~[3, 3].  */
  if (n1 == 3)
    n1 = 4;
  void *p = alloc_int (n1);

  /* Set N2 to the anti-range ~[7, 7].  */
  if (n2 == 7)
    n2 = 8;

  access_int (p, n2);
}


void nowarn_alloc_access_same_anti_range_uint (unsigned n)
{
  /* Set N to the anti-range ~[3, 3].  */
  if (n == 3)
    n = 4;
  void *p = alloc_uint (n);

  /* Verify there is no warning for an access to N bytes at P.
     This means the warning has to assume the value of N in the call
     to alloc() is in the larger subrange [4, UINT_MAX], while in
     the call to access() in [0, 3].  */
  access_uint (p, n);
}

/* Same as above but with two valid ranges.  */

void nowarn_alloc_access_anti_range_uint (unsigned n1, unsigned n2)
{
  /* Set N1 to the anti-range ~[3, 3].  */
  if (n1 == 3)
    n1 = 4;
  void *p = alloc_uint (n1);

  /* Set N2 to the anti-range ~[7, 7].  */
  if (n2 == 7)
    n2 = 8;

  access_uint (p, n2);
}


void* nowarn_malloc_anti_range_memset_range (size_t n1, size_t n2)
{
  /* Set N1 to the anti-range ~[3, 3].  */
  if (n1 == 3)
    n1 = 4;
  void *p = malloc (n1);

  /* Set N2 to the range [5, MAX].  */
  if (n2 < 5)
    n2 = 5;
  return memset (p, 0, n2);
}

void* nowarn_malloc_range_bzero_anti_range (size_t n1, size_t n2)
{
  /* Set N1 to the anti-range ~[3, 3].  */
  if (n1 > 4)
    n1 = 4;
  void *p = malloc (n1);

  /* Set N2 to the range [5, MAX].  */
  if (n2 <= 3 || 5 <= n2)
    n2 = 4;
  bzero (p, n2);
  return p;
}
