// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options "-fschedule-insns2 -fsel-sched-pipelining -fselective-scheduling2 -funroll-all-loops -march=core2" }

void bar ();

void foo ()
{
  for (;;)
    bar ();
}
