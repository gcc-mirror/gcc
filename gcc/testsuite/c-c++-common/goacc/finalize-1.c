/* Test valid usage and processing of the finalize clause.  */

/* { dg-additional-options "-fdump-tree-original -fdump-tree-gimple" } */

extern int del_r;
extern float del_f[3];
extern char *del_f_p;
extern double cpo_r[8];
extern long cpo_f;
extern char *cpo_f_p;

void f ()
{
#pragma acc exit data delete (del_r)
/* { dg-final { scan-tree-dump-times "(?n)#pragma acc exit data map\\(release:del_r\\);$" 1 "original" } }
   { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_enter_exit_data map\\(release:del_r \\\[len: \[0-9\]+\\\]\\)$" 1 "gimple" } } */

#pragma acc exit data finalize delete (del_f)
/* { dg-final { scan-tree-dump-times "(?n)#pragma acc exit data map\\(release:del_f\\) finalize;$" 1 "original" } }
   { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_enter_exit_data map\\(delete:del_f \\\[len: \[0-9\]+\\\]\\) finalize$" 1 "gimple" } } */

#pragma acc exit data finalize delete (del_f_p[2:5])
/* { dg-final { scan-tree-dump-times "(?n)#pragma acc exit data map\\(release:\\*\\(del_f_p \\+ 2\\) \\\[len: 5\\\]\\) map\\(firstprivate:del_f_p \\\[pointer assign, bias: 2\\\]\\) finalize;$" 1 "original" } }
   { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_enter_exit_data map\\(delete:\[^ \]+ \\\[len: 5\\\]\\) finalize$" 1 "gimple" } } */

#pragma acc exit data copyout (cpo_r)
/* { dg-final { scan-tree-dump-times "(?n)#pragma acc exit data map\\(from:cpo_r\\);$" 1 "original" } }
   { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_enter_exit_data map\\(from:cpo_r \\\[len: \[0-9\]+\\\]\\)$" 1 "gimple" } } */

#pragma acc exit data copyout (cpo_f) finalize
/* { dg-final { scan-tree-dump-times "(?n)#pragma acc exit data finalize map\\(from:cpo_f\\);$" 1 "original" } }
   { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_enter_exit_data finalize map\\(force_from:cpo_f \\\[len: \[0-9\]+\\\]\\)$" 1 "gimple" } } */

#pragma acc exit data copyout (cpo_f_p[4:10]) finalize
/* { dg-final { scan-tree-dump-times "(?n)#pragma acc exit data finalize map\\(from:\\*\\(cpo_f_p \\+ 4\\) \\\[len: 10\\\]\\) map\\(firstprivate:cpo_f_p \\\[pointer assign, bias: 4\\\]\\);$" 1 "original" } }
   { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_enter_exit_data finalize map\\(force_from:\[^ \]+ \\\[len: 10\\\]\\)$" 1 "gimple" } } */
}
