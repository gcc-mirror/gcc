/* { dg-do compile { target x86_64-*-* } } */
/* { dg-additional-options "-fdump-tree-gimple -foffload=disable" } */

/* Test merging of context selectors for nested "begin declare variant"
   directives.

   The OpenMP 6.0 spec says: "the effective context selectors of the outer
   directive are appended do the context selector of the inner directive to
   form the effective context selector of the inner directive.  If a
   trait-set-selector is present on both directives, the trait-selector list of
   the outer directive is appended to the trait-selector list of the inner
   directive after equivalent trait-selectors have been removed from the outer
   list."  */

int f1 (int x) { return x; }
int f2 (int x) { return x; }
int f3 (int x) { return x; }
int f4 (int x) { return x; }
int f5 (int x) { return x; }

/* Check that duplicate traits can be detected, even when the properties
   use different forms.  (If these were considered different, it would
   trigger an error instead.)  */
#pragma omp begin declare variant match (implementation={vendor(gnu)})
#pragma omp begin declare variant match (implementation={vendor("gnu")})
int f1 (int x) { return -x; }
#pragma omp end declare variant
#pragma omp end declare variant

#pragma omp begin declare variant match (implementation={vendor("gnu")})
#pragma omp begin declare variant match (implementation={vendor(gnu)})
int f2 (int x) { return -x; }
#pragma omp end declare variant
#pragma omp end declare variant

/* Check that non-duplicate traits are collected from both inner and outer.  */

#pragma omp begin declare variant match (device={kind("host")})
#pragma omp begin declare variant match (device={arch("x86")})
int f3 (int x) { return -x; }
#pragma omp end declare variant
#pragma omp end declare variant
/* { dg-final { scan-tree-dump "f3\\.ompvariant.*kind \\(.host.\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "f3\\.ompvariant.*arch \\(.x86.\\)" "gimple" } } */

/* Check that traits for construct selectors merge as expected.  */

#pragma omp begin declare variant match (construct={parallel, for})
#pragma omp begin declare variant match (construct={teams})
int f4 (int x) { return -x; }
#pragma omp end declare variant
#pragma omp end declare variant
/* { dg-final { scan-tree-dump "f4\\.ompvariant.*teams, parallel, for" "gimple" }  } */

/* Check that multiple trait sets are collected.  */

extern int flag;

#pragma omp begin declare variant match (construct={parallel, for})
#pragma omp begin declare variant match (construct={teams})
#pragma omp begin declare variant match (user={condition(flag)})
#pragma omp begin declare variant match (device={kind("host")})
int f5 (int x) { return -x; }
#pragma omp end declare variant
#pragma omp end declare variant
#pragma omp end declare variant
#pragma omp end declare variant
/* { dg-final { scan-tree-dump "f5\\.ompvariant.*teams, parallel, for" "gimple" }  } */
/* { dg-final { scan-tree-dump "f5\\.ompvariant.*flag" "gimple" }  } */
/* { dg-final { scan-tree-dump "f5\\.ompvariant.*kind \\(.host.\\)" "gimple" } } */
