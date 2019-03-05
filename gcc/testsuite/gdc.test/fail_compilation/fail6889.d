/*
TEST_OUTPUT:
---
fail_compilation/fail6889.d(16): Error: cannot goto out of `scope(success)` block
fail_compilation/fail6889.d(17): Error: cannot goto in to `scope(success)` block
fail_compilation/fail6889.d(19): Error: return statements cannot be in `scope(success)` bodies
fail_compilation/fail6889.d(23): Error: continue is not inside `scope(success)` bodies
fail_compilation/fail6889.d(24): Error: break is not inside `scope(success)` bodies
fail_compilation/fail6889.d(29): Error: continue is not inside `scope(success)` bodies
fail_compilation/fail6889.d(30): Error: break is not inside `scope(success)` bodies
---
*/
void test_success()
{
L1:
    scope(success) { L2: goto L1; } // NG
    goto L2;                        // NG

    scope(success) { return; }      // NG (from fail102.d)

    foreach (i; 0..1)
    {
        scope(success) continue;    // NG
        scope(success) break;       // NG
    }

    foreach (i; Aggr())
    {
        scope(success) continue;    // NG
        scope(success) break;       // NG
    }
  /+
    // is equivalent with:
    switch (
      Aggr().opApply((int i){
        scope(success) return 0;    // NG
        scope(success) return 1;    // NG
        return 0;
      }))
    {
        default: break;
    }
  +/
}

/*
TEST_OUTPUT:
---
fail_compilation/fail6889.d(56): Error: cannot goto in to `scope(failure)` block
---
*/
void test_failure()
{
L1:
    scope(failure) { L2: goto L1; } // OK
    goto L2;                        // NG

    scope(failure) { return; }      // OK

    foreach (i; 0..1)
    {
        scope(failure) continue;    // OK
        scope(failure) break;       // OK
    }

    foreach (i; Aggr())
    {
        scope(failure) continue;    // OK
        scope(failure) break;       // OK
    }
  /+
    // is equivalent with:
    switch (
      Aggr().opApply((int i){
        scope(failure) return 0;    // OK
        scope(failure) return 1;    // OK
        return 0;
      }))
    {
        default: break;
    }
  +/
}

/*
TEST_OUTPUT:
---
fail_compilation/fail6889.d(100): Error: cannot goto out of `scope(exit)` block
fail_compilation/fail6889.d(101): Error: cannot goto in to `scope(exit)` block
fail_compilation/fail6889.d(103): Error: return statements cannot be in `scope(exit)` bodies
fail_compilation/fail6889.d(107): Error: continue is not inside `scope(exit)` bodies
fail_compilation/fail6889.d(108): Error: break is not inside `scope(exit)` bodies
fail_compilation/fail6889.d(113): Error: continue is not inside `scope(exit)` bodies
fail_compilation/fail6889.d(114): Error: break is not inside `scope(exit)` bodies
---
*/
void test_exit()
{
L1:
    scope(exit) { L2: goto L1; }    // NG
    goto L2;                        // NG

    scope(exit) { return; }         // NG (from fail102.d)

    foreach (i; 0..1)
    {
        scope(exit) continue;       // NG
        scope(exit) break;          // NG
    }

    foreach (i; Aggr())
    {
        scope(exit) continue;       // NG
        scope(exit) break;          // NG
    }
  /+
    // is equivalent with:
    switch (
      Aggr().opApply((int i){
        scope(exit) return 0;       // NG
        scope(exit) return 1;       // NG
        return 0;
      }))
    {
        default: break;
    }
  +/
}

struct Aggr { int opApply(int delegate(int) dg) { return dg(0); } }
