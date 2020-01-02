/* PR middle-end/91582 - missing heap overflow detection for strcpy
   Verify calls via function pointers.
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-array-bounds -ftrack-macro-expansion=0" } */

typedef __attribute__ ((alloc_size (1))) char* allocfn_t (unsigned);

extern allocfn_t allocfn;

void sink (void*);

void direct_call (void)
{
  char *q = allocfn (0);            // { dg-message "at offset 0 to an object with size 0 allocated by 'allocfn'" }
  q[0] = 0;                         // { dg-warning "\\\[-Wstringop-overflow" }
  sink (q);
}


void local_ptr_call (void)
{
  allocfn_t *ptr = allocfn;
  char *q = ptr (1);                // { dg-message "at offset -1 to an object with size 1 allocated by 'allocfn'" }
  q[0] = 0;
  q[-1] = 0;                        // { dg-warning "\\\[-Wstringop-overflow" }
  sink (q);
}


void global_ptr_call (void)
{
  extern allocfn_t *ptralloc;

  allocfn_t *ptr = ptralloc;
  char *q = ptr (2);               // { dg-message "at offset 3 to an object with size 2 allocated by 'ptralloc'" }
  q[0] = 0;
  q[1] = 1;
  q[3] = 3;                        // { dg-warning "\\\[-Wstringop-overflow" }
  sink (q);
}

void global_ptr_array_call (void)
{
  extern allocfn_t * (arralloc[]);

  allocfn_t *ptr = arralloc[0];
  char *q = ptr (2);               // { dg-message "at offset 3 to an object with size 2 allocated by 'ptr'" }
  q[0] = 1;
  q[1] = 2;
  q[3] = 3;                        // { dg-warning "\\\[-Wstringop-overflow" }
  sink (q);
}


struct S { allocfn_t *ptralloc; };

void member_ptr_call (struct S *p)
{
  char *q = p->ptralloc (3);       // { dg-message "at offset 5 to an object with size 3 allocated by 'ptralloc' here" }
  q[0] = 0;
  q[1] = 1;
  q[2] = 2;
  q[5] = 0;                        // { dg-warning "\\\[-Wstringop-overflow" }
  sink (q);
}

