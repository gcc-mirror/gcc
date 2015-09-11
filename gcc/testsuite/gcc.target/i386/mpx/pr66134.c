/* { dg-do compile } */
/* { dg-options "-O2 -fcheck-pointer-bounds -mmpx -fno-tree-ccp" } */

extern int vfork (void) __attribute__ ((__nothrow__ , __leaf__));
void test1 (void);
void test2 (void);
void test3 (int *);

void test (int *p)
{
 test1 ();
 p++;
 test2 ();
 p++;
 vfork ();
 test3 (p);
}
