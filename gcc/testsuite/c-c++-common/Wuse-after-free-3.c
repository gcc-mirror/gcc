/* Exercise -Wuse-after-free with user-defined deallocators.
   { dg-do compile }
   { dg-options "-O0 -Wall" } */

typedef __SIZE_TYPE__ size_t;

#if __cplusplus
#  define EXTERN_C extern "C"
#else
#  define EXTERN_C extern
#endif

#define A(...) __attribute__ ((malloc (__VA_ARGS__)))

EXTERN_C void free (void *);
EXTERN_C void* realloc (void *, size_t);

typedef struct List { struct List *next; } List;

// User-defined allocator/deallocator just like like realloc and free.
extern                     void  list_free (List *);
extern                     List* list_realloc (size_t, List *);
extern A (list_realloc, 2) List* list_realloc (size_t, List *);
extern A (list_free, 1)    List* list_realloc (size_t, List *);


void sink (void *);

extern int ei;
extern List *elp, *elpa[];

void nowarn_list_free (struct List *lp)
{
  {
    list_free (lp);
    lp = 0;
    sink (lp);
  }
  {
    list_free (elp);
    elp = 0;
    sink (elp);
  }
  {
    list_free (elpa[0]);
    elpa[0] = 0;
    sink (elpa[0]);
  }
  {
    void *vp = elpa[0];
    list_free (elpa[0]);
    sink (vp);
  }
  {
    List *p = elpa[1];
    if (ei & 1)
      list_free (p);
    if (ei & 2)
      sink (p);
  }
  {
    struct List *next = lp->next;
    list_free (lp);
    list_free (next);
  }
}


void nowarn_list_free_list (List *head)
{
  for (List *p = head, *q; p; p = q)
    {
      q = p->next;
      list_free (p);
    }
}

void warn_list_free_list (List *head)
{
  List *p = head;
  for (; p; p = p->next)      // { dg-warning "\\\[-Wuse-after-free" }
    list_free (p);            // { dg-message "call to '\(void \)?list_free\(\\(List\\*\\)\)?'" "note" }
}
