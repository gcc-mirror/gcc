/* { dg-do compile } */
/* { dg-options "-Os -c -fdump-ipa-inline -fno-early-inlining -fno-partial-inlining -fno-ipa-cp"  } */
/* { dg-add-options bind_pic_locally } */

void work_hard (void);

void do_something (int shall_i_work)
{
  if (shall_i_work)
    {
      work_hard ();
      work_hard ();
      work_hard ();
      work_hard ();
      work_hard ();
      work_hard ();
      work_hard ();
      work_hard ();
    }
}
int foo (int invariant)
{
  do_something (0);
  do_something (1);
}


/* We should inline do_something(0),  but not do_something (1).  */
/* { dg-final { scan-ipa-dump "Inlined 1 calls, eliminated 0 functions"  "inline"  } } */
/* Call to work_hard should be detected as optimized out.  */
/* { dg-final { scan-ipa-dump-times "predicate: .false." 8 "inline"  } } */
