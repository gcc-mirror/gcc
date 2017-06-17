/* The function below expands to a loop whose latch block starts with
   a PHI node and the corresponding debug stmt.  In RTL, there are no
   PHI nodes, but the debug insn that references the incoming k
   remains, even though one of the incoming edges has it
   uninitialized.  After unrolling, however, the debug insn becomes
   unconditional, and this exposed a problem in the webizer.  Because
   DF doesn't combine the uses of an uninitialized pseudo into a
   single UD chain, we created a separate web for each use.
   Allocating separate registers or stack slots for each uninitialized
   use is wasteful, but the problem became more apparent in
   -fcompare-debug tests: register numbers went out of sync, and could
   have caused codegen differences depending on whether or not the
   debug insns were present.  The fix was to arrange for web to
   combine uninitialized uses into a single web.  */

/* { dg-do compile } */
/* { dg-options "-g -O1 -funroll-loops -fcompare-debug" } */
/* { dg-xfail-if "" { powerpc-ibm-aix* } } */

void foo()
{
  unsigned k;
  while (--k > 0);
}
