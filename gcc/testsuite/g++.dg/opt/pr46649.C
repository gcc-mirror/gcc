// { dg-do compile { target powerpc*-*-* ia64-*-* x86_64-*-* } }
// { dg-options "-fschedule-insns -fselective-scheduling" }

void foo ()
{
  for (;;)
    for (;;({break;}))
    ;
}                 
