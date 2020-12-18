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

// { dg-final { scan-tree-dump-not "__attribute__\\\(\\\(omp declare target\\\)\\\)\\\nfoo \\\(\\\)" "gimple" } }
// { dg-final { scan-tree-dump "__attribute__\\\(\\\(omp declare target\\\)\\\)\\\nbar \\\(\\\)" "gimple" } }
// { dg-final { scan-tree-dump "__attribute__\\\(\\\(omp declare target\\\)\\\)\\\nbaz \\\(\\\)" "gimple" } }
// { dg-final { scan-tree-dump "__attribute__\\\(\\\(omp declare target\\\)\\\)\\\nqux \\\(\\\)" "gimple" } }
// { dg-final { scan-assembler-not "\\\.offload_var_table:\\n.+\\\.quad\\s+a" } }
// { dg-final { scan-assembler "\\\.offload_var_table:\\n.+\\\.quad\\s+b" } }
// { dg-final { scan-assembler "\\\.offload_var_table:\\n.+\\\.quad\\s+c" } }
// { dg-final { scan-assembler "\\\.offload_var_table:\\n.+\\\.quad\\s+d" } }
// { dg-final { scan-assembler "\\\.offload_var_table:\\n.+\\\.quad\\s+e" } }
// { dg-final { scan-assembler "\\\.offload_var_table:\\n.+\\\.quad\\s+f" } }
// { dg-final { scan-assembler "\\\.offload_var_table:\\n.+\\\.quad\\s+g" } }
