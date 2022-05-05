/* Exercise basic cases of -Wuse-after-free without optimization.
   { dg-do compile }
   { dg-options "-O0 -Wall" } */

typedef __SIZE_TYPE__ size_t;

#if __cplusplus
#  define EXTERN_C extern "C"
#else
#  define EXTERN_C extern
#endif

EXTERN_C void* alloca (size_t);

EXTERN_C void* calloc (size_t, size_t);
EXTERN_C void* malloc (size_t);

EXTERN_C void free (void*);


void sink (void *);

extern void* evp;
extern void* evpa[];

extern int ei;

struct List { struct List *next; };

void nowarn_free (void *vp, struct List *lp)
{
  {
    free (vp);
    vp = 0;
    sink (vp);
  }
  {
    free (evp);
    evp = 0;
    sink (evp);
  }
  {
    free (evpa[0]);
    evpa[0] = 0;
    sink (evpa[0]);
  }
  {
    void *vp = evpa[0];
    free (evpa[1]);
    sink (vp);
  }
  {
    void *p = evpa[1];
    if (ei & 1)
      free (p);
    if (ei & 2)
      sink (p);
  }
  {
    struct List *next = lp->next;
    free (lp);
    free (next);
  }
}

void nowarn_free_arg (void *p, void *q)
{
  free (p);
  if (q)
    free (q);
}

void nowarn_free_extern (void)
{
  extern void *ep, *eq;
  free (ep);
  ep = eq;
  free (ep);
}

void nowarn_free_assign (void)
{
  extern void *ep;
  free (ep);
  ep = 0;
  free (ep);
}

#pragma GCC diagnostic push
/* Verify that -Wuse-after-free works with #pragma diagnostic.  Note
   that the option name should not need to include a trailing =, even
   though it's a multi-level option.  (specifying the level after
   the option, as in "-Wuse-after-free=2", doesn't work.  */
#pragma GCC diagnostic ignored "-Wuse-after-free"

void nowarn_double_free_suppressed (void *p)
{
  free (p);
  free (p);
}

#pragma GCC diagnostic pop

void warn_double_free_arg (void *p)
{
  free (p);                   // { dg-message "call to '\(void \)?free\(\\(void\\*\\)\)?'" "note" }
  // Verify exactly one warning is issued.
  free (p);                   // { dg-warning "\\\-Wuse-after-free" }
                              // { dg-bogus "\\\-Wuse-after-free" "duplicate warning" { target *-*-* } .-1 }

}

void warn_double_free_extern (void)
{
  /* GCC assumes free() clobbers global memory and the warning is
     too simplistic to see through that assumption.  */
  extern void *ep, *eq;
  {
    eq = ep;
    free (ep);                // { dg-message "call to 'free'" "pr??????" { xfail *-*-* } }
    free (eq);                // { dg-warning "\\\-Wuse-after-free" "pr??????" { xfail *-*-* } }
  }
}

void warn_deref_after_free (int *p, int i)
{
  int *q0 = p, *q1 = p + 1, *qi = p + i;
  free (p);                   // { dg-message "call to '\(void \)?free\(\\(void\\*\\)\)?'" "note" }
  *p = 0;                     // { dg-warning "\\\-Wuse-after-free" }

  *q0 = 0;                    // { dg-warning "\\\-Wuse-after-free" }
  *q1 = 0;                    // { dg-warning "\\\-Wuse-after-free" }
  *qi = 0;                    // { dg-warning "\\\-Wuse-after-free" }
}

void warn_array_ref_after_free (int *p, int i)
{
  free (p);                   // { dg-message "call to '\(void \)?free\(\\(void\\*\\)\)?'" "note" }
  p[i] = 0;                   // { dg-warning "\\\-Wuse-after-free" }
}

void nowarn_free_list (struct List *head)
{
  for (struct List *p = head, *q; p; p = q)
    {
      q = p->next;
      free (p);
    }
}

void warn_free_list (struct List *head)
{
  struct List *p = head;
  for (; p; p = p->next)      // { dg-warning "\\\[-Wuse-after-free" }
    free (p);                 // { dg-message "call to '\(void \)?free\(\\(void\\*\\)\)?'" "note" }
}


void warn_free (void *vp)
{
  {
    free (vp);                // { dg-message "call to '\(void \)?free\(\\(void\\*\\)\)?'" "note" }
    evp = vp;                 // { dg-warning "-Wuse-after-free" }
    evpa[0] = vp;             // { dg-warning "-Wuse-after-free" }
    evpa[1] = evp;
  }
}
