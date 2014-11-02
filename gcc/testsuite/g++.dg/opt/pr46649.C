// { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } }
// { dg-options "-fschedule-insns -fselective-scheduling" }

void foo ()
{
  for (;;)
    for (;;({break;}))
    ;
}                 
