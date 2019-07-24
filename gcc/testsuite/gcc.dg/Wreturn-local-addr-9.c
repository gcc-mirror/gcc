/* PR c/71924 - missing -Wreturn-local-addr returning alloca result
   Test derived from gcc.c-torture/execute/20071108-1.c.  It shows
   a false positive at -Os caused by the jump threading/vrp1 pass.
   { dg-do compile }
   { dg-options "-Os -fdump-tree-optimized" } */

struct S
{
  int i;
};

void* f (void);

__attribute__ ((noinline))
struct S* g (int i)
{
  struct S *p = f (), q;

  if (p == 0)
    p = &q;

  p->i = i;

  if (p == &q)
    p = 0;

  /* With -Os the warning pass sees:

       ...
       <bb 4>
       # p_1 = PHI <&q(2), p_5(3)>
       p_1->i = i_6(D);
       if (&q == p_1)
         goto <bb 6>; [14.90%]
       else
         goto <bb 5>; [85.10%]

       <bb 5>

       <bb 6>
       # p_2 = PHI <0B(4), p_1(5)>
       q ={v} {CLOBBER};
       return p_2;
     }

     which leads to:  */
  return p;         /* { dg-bogus "may return address of local variable" "" { xfail *-*-* } } */

  /* Whereas as -O2 the pass sees:

       <bb 2>
       p_5 = f ();
       if (p_5 == 0B)
         goto <bb 4>; [30.00%]
       else
         goto <bb 3>; [70.00%]

       <bb 3>
       # p_2 = PHI <0B(5), p_5(4)>
       q ={v} {CLOBBER};
       return p_2;

       <bb 4>
       p_5->i = i_6(D);
       goto <bb 3>; [100.00%]

       <bb 5>
       q.i = i_6(D);
       goto <bb 3>; [100.00%]
     }

     and no warning.  */
}
