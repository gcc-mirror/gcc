/* { dg-do compile } */
/* { dg-options "-O3" } */

/* Test provided by Volker Reichelt in PR 14553.  The redundant PHI
   node elimination pass was not using the right API functions to
   propagate pointers, which resulted in dereferenced pointers that
   did not have memory tags associated with them.  */

void foo(int* p)
{
    int i;
    for (i=1; i>0; --i, ++p)
        *p=0;
}

void bar(int* p) { foo(p); }
