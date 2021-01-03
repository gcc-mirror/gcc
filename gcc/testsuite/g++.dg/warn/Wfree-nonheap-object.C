/* PR ????? - No warning on attempts to access free object
   Verify that attempts to deallocate objects by pointers with nonzero
   offsets is diagnosed.
   { dg-do compile }
   { dg-options "-O2 -Wall -Wfree-nonheap-object" }  */

typedef __INTPTR_TYPE__ intptr_t;
typedef __SIZE_TYPE__   size_t;

void sink (void*, ...);

extern char ecarr[];
extern void* eparr[];

extern char *eptr;

char* source (void);

void nowarn_op_delete (void *p, void ***ppp, size_t n, intptr_t iptr)
{
  operator delete (p);

  p = 0;
  operator delete (p);

  p = operator new (n);
  sink (p);
  operator delete (p);

  p = operator new (n);
  sink (p);

  operator delete ((void*)iptr);

  p = source ();
  operator delete (p);

  p = source ();
  p = (char*)p - 1;
  operator delete (p);

  operator delete (**ppp);
  operator delete (*ppp);
  operator delete (ppp);
}

void warn_op_delete_cstaddr (void *p)
{
  operator delete (p);
  p = (void*)~0;
  operator delete (p);        // { dg-warning "called on a pointer to an unallocated object" } */
}

void warn_op_delete_funcaddr ()
{
  void *p = (void*)&warn_op_delete_funcaddr;
  operator delete (p);        // { dg-warning "called on unallocated object 'void warn_op_delete_funcaddr()" } */
}

void warn_op_delete_string (void *p)
{
  operator delete (p);
  p = (void*)"";
  operator delete (p);        // { dg-warning "called on a pointer to an unallocated object" } */
}

void warn_op_delete_ptr_to_self (void *p)
{
  operator delete (p);
  p = &p;
  operator delete (p);        // { dg-warning "called on unallocated object 'p'" } */
}

void nowarn_op_new_delete (size_t n)
{
  void *p = operator new (n);
  sink (p);
  operator delete (p);
}

void nowarn_op_new_delete_ptr_plus (size_t n)
{
  void *p0_1 = operator new (n);
  void *p1 = (char*)p0_1 + 1;
  sink (p0_1, p1);
  void *p0_2 = (char*)p1 - 1;
  sink (p0_1, p1, p0_2);
  operator delete (p0_2);
}

void warn_op_new_delete_cstoff (size_t n)
{
  void *p = operator new (n);
  void *q = (char*)p + 1;
  sink (p, q);
  operator delete (q);        // { dg-warning "'void operator delete\\\(void\\\*\\\)' called on pointer '\[^'\]+' with nonzero offset 1" }
}

void warn_op_new_delete_ptr_plus (size_t n)
{
  char *p = (char*)operator new (n);
  sink (++p);
  operator delete (p);        // { dg-warning "called on pointer '\[^']+' with nonzero offset 1" }
}

void warn_op_delete_funcret_plus (size_t n)
{
  char *p = source ();
  sink (++p);
  operator delete (p);        // { dg-warning "called on pointer '\[^']+' with nonzero offset 1" }
}

void warn_op_delete_eptr_plus (int i)
{
  extern char *ecp;

  if (i < 1)
    i = 1;

  char *p = ecp + i;
  sink (p);

  operator delete (p);        // { dg-warning "called on pointer '\[^']+' with nonzero offset \\\[1, \\d+]" }
}
