/* Exercise basic C++ only cases of -Wuse-after-free without optimization.
   { dg-do compile }
   { dg-options "-O0 -Wall" } */

typedef __SIZE_TYPE__ size_t;

extern "C" void free (void *);
extern "C" void* realloc (void *, size_t);

void sink (void *);

extern void* evp;
extern void* evpa[];

extern int ei;

struct List { struct List *next; };

void nowarn_delete (void *vp, struct List *lp)
{
  {
    operator delete (vp);
    vp = 0;
    sink (vp);
  }
  {
    operator delete (evp);
    evp = 0;
    sink (evp);
  }
  {
    operator delete (evpa[0]);
    evpa[0] = 0;
    sink (evpa[0]);
  }
  {
    void *vp = evpa[0];
    operator delete (evpa[0]);
    sink (vp);
  }
  {
    void *p = evpa[1];
    if (ei & 1)
      operator delete (p);
    if (ei & 2)
      sink (p);
  }
  {
    struct List *next = lp->next;
    operator delete (lp);
    operator delete (next);
  }
}

void nowarn_delete_arg (void *p, void *q)
{
  operator delete (p);
  if (q)
    operator delete (q);
}

void nowarn_delete_extern (void)
{
  extern void *ep, *eq;
  operator delete (ep);
  ep = eq;
  operator delete (ep);
}

void nowarn_delete_assign (void)
{
  extern void *ep;
  operator delete (ep);
  ep = 0;
  operator delete (ep);
}

void warn_double_delete_arg (void *p)
{
  operator delete (p);        // { dg-message "call to 'void operator delete\\(void\\*\\)'" "note" }
  operator delete (p);        // { dg-warning "\\\-Wuse-after-free" }
}

void warn_delete_free_arg (void *p)
{
  operator delete (p);        // { dg-message "call to 'void operator delete\\(void\\*\\)'" "note" }
  free (p);                   // { dg-warning "\\\-Wuse-after-free" }
}

void warn_free_delete_arg (void *p)
{
  free (p);                   // { dg-message "call to 'void free\\(void\\*\\)'" "note" }
  operator delete (p);        // { dg-warning "\\\-Wuse-after-free" }
}

void warn_mismatched_double_delete_arg (void *p, void *q)
{
  operator delete (p);        // { dg-message "call to 'void operator delete\\(void\\*\\)'" "note" }
  operator delete[] (p);      // { dg-warning "\\\-Wuse-after-free" }

  operator delete[] (q);      // { dg-message "call to 'void operator delete \\\[]\\(void\\*\\)'" "note" }
  operator delete (q);        // { dg-warning "\\\-Wuse-after-free" }
}

void warn_double_delete_extern (void)
{
  /* GCC assumes operator delete() clobbers global memory and the warning is
     too simplistic to see through that assumption.  */
  extern void *ep, *eq;
  {
    eq = ep;
    operator delete (ep);     // { dg-message "call to 'operator delete'" "pr??????" { xfail *-*-* } }
    operator delete (eq);     // { dg-warning "\\\-Wuse-after-free" "pr??????" { xfail *-*-* } }
  }
}

void warn_deref_after_delete (int *p, int i)
{
  int *q0 = p, *q1 = p + 1, *qi = p + i;
  operator delete (p);        // { dg-message "call to 'void operator delete\\(void\\*\\)'" "note" }
  *p = 0;                     // { dg-warning "\\\-Wuse-after-free" }

  *q0 = 0;                    // { dg-warning "\\\-Wuse-after-free" }
  *q1 = 0;                    // { dg-warning "\\\-Wuse-after-free" }
  *qi = 0;                    // { dg-warning "\\\-Wuse-after-free" }
}

void warn_array_ref_after_delete (int *p, int i)
{
  operator delete (p);        // { dg-message "call to 'void operator delete\\(void\\*\\)'" "note" }
  p[i] = 0;                   // { dg-warning "\\\-Wuse-after-free" }
}

void nowarn_delete_list (struct List *head)
{
  for (struct List *p = head, *q; p; p = q)
    {
      q = p->next;
      operator delete (p);
    }
}

void warn_delete_list (struct List *head)
{
  struct List *p = head;
  for (; p; p = p->next)      // { dg-warning "\\\[-Wuse-after-free" }
    operator delete (p);      // { dg-message "call to 'void operator delete\\(void\\*\\)'" "note" }
}

void warn_delete (void *vp)
{
  {
    operator delete (vp);     // { dg-message "call to 'void operator delete\\(void\\*\\)'" "note" }
    evp = vp;                 // { dg-warning "-Wuse-after-free" }
    evpa[0] = vp;             // { dg-warning "-Wuse-after-free" }
    evpa[1] = evp;
  }
}
