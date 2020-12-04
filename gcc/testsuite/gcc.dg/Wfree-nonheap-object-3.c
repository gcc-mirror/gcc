/* PR ????? - No warning on attempts to access free object
   Verify that freeing unallocated objects referenced indirectly through
   pointers obtained from function calls is diagnosed.
   { dg-do compile }
   { dg-options "-O2 -Wall -Wfree-nonheap-object" }  */

typedef __SIZE_TYPE__ size_t;

extern void free (void*);
extern char* memchr (const void*, int, size_t);
extern char* strchr (const char*, int);

void sink (void*, ...);

extern char ecarr[];
extern void* eparr[];

extern char *eptr;


void warn_free_memchr_ecarr (int x, size_t n)
{
  char *p = memchr (ecarr, x, n);
  sink (p);
  free (p);                   // { dg-warning "\\\[-Wfree-nonheap-object" }
}

void warn_free_memchr_ecarr_offset (int i, int j, int x, size_t n)
{
  char *p = memchr (ecarr + i, x, n);
  char *q = p + j;
  sink (p, q);
  free (q);                   // { dg-warning "\\\[-Wfree-nonheap-object" }
}


void warn_free_memchr_local_arr (int x, size_t n)
{
  char a[8];
  sink (a);

  char *p = memchr (a, x, n);
  sink (p);
  free (p);                   // { dg-warning "\\\[-Wfree-nonheap-object" }
}

void warn_free_memchr_local_arr_offset (int i, int j, int x, size_t n)
{
  char a[8];
  sink (a);

  char *p = memchr (a + i, x, n);
  char *q = p + j;
  sink (p, q);
  free (q);                   // { dg-warning "\\\[-Wfree-nonheap-object" }
}

