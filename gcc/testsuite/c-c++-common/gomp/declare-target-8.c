/* { dg-do compile } */
/* { dg-options "-fopenmp" } */
// { dg-additional-options "-Wno-deprecated-openmp" }

int var1;
#pragma omp declare target link(var1)
#pragma omp declare target local(var1) /* { dg-error "'var1' specified both in declare target 'link' and 'local' clauses" } */

int var2;
#pragma omp declare target local(var2)
#pragma omp declare target link(var2) /* { dg-error "'var2' specified both in declare target 'link' and 'local' clauses" } */

int var3;
#pragma omp declare target enter(var3)
#pragma omp declare target local(var3) /* { dg-error "'var3' specified both in declare target 'local' and 'to' or 'enter' clauses" } */

int var4;
#pragma omp declare target local(var4)
#pragma omp declare target enter(var4) /* { dg-error "'var4' specified both in declare target 'local' and 'enter' clauses" } */
#pragma omp declare target to(var4) /* { dg-error "'var4' specified both in declare target 'local' and 'to' clauses" } */

#pragma omp begin declare target
int var5;
#pragma omp declare target local(var5) /* { dg-error "'var5' specified both in declare target 'local' and 'to' or 'enter' clauses" } */
#pragma omp end declare target

int var6, var7;
#pragma omp declare target local(var6) local(var6) /* { dg-error "'var6' appears more than once on the same 'declare target' directive" } */
#pragma omp declare target local(var7) enter(var7) /* { dg-error "'var7' appears more than once on the same 'declare target' directive" } */
