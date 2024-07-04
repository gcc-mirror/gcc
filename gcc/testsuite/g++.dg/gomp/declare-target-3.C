// { dg-do compile }
// { dg-additional-options "-fdump-tree-gimple" }

// Test implicit marking of declare target to.

int foo () { return 1; }
int bar () { return 2; }	// Implicitly marked (due to b)
int baz () { return 3; }	// Implicitly marked (due to d via c)
int qux () { return 4; }	// Implicitly marked (due to g via f and e)

int a = foo ();
int b = bar ();	// Explicitly marked
int c = baz ();	// Implicitly marked (due to d)
int *d = &c;	// Explicitly marked
int e = qux ();	// Implicitly marked (due to g via f)
int f = e + 1;	// Implicitly marked (due to g)
int *g = &f;	// Explicitly marked

#pragma omp declare target to(b, d, g)

// { dg-final { scan-tree-dump-not "__attribute__\\\(\\\(omp declare target\\\)\\\)\\\nint foo \\\(\\\)" "gimple" } }
// { dg-final { scan-tree-dump "__attribute__\\\(\\\(omp declare target\\\)\\\)\\\nint bar \\\(\\\)" "gimple" } }
// { dg-final { scan-tree-dump "__attribute__\\\(\\\(omp declare target\\\)\\\)\\\nint baz \\\(\\\)" "gimple" } }
// { dg-final { scan-tree-dump "__attribute__\\\(\\\(omp declare target\\\)\\\)\\\nint qux \\\(\\\)" "gimple" } }
// { dg-final { scan-assembler-not "\\\.offload_var_table:\\n.+\\\.(quad|long)\\s+a" { target { offloading_enabled } } } }
// { dg-final { scan-assembler "\\\.offload_var_table:\\n.+\\\.(quad|long)\\s+b" { target { offloading_enabled } } } }
// { dg-final { scan-assembler "\\\.offload_var_table:\\n.+\\\.(quad|long)\\s+c" { target { offloading_enabled } } } }
// { dg-final { scan-assembler "\\\.offload_var_table:\\n.+\\\.(quad|long)\\s+d" { target { offloading_enabled } } } }
// { dg-final { scan-assembler "\\\.offload_var_table:\\n.+\\\.(quad|long)\\s+e" { target { offloading_enabled } } } }
// { dg-final { scan-assembler "\\\.offload_var_table:\\n.+\\\.(quad|long)\\s+f" { target { offloading_enabled } } } }
// { dg-final { scan-assembler "\\\.offload_var_table:\\n.+\\\.(quad|long)\\s+g" { target { offloading_enabled } } } }
