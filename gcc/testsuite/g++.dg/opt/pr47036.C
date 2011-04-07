// { dg-do compile { target powerpc*-*-* ia64-*-* x86_64-*-* } }
// { dg-options "-fschedule-insns -fselective-scheduling -fno-dce" }


void foo ()
{
  for (;;)
    for (;;({break;}));
}

