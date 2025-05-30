/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

struct S {
  int x,y;
};

#pragma omp declare mapper(default : struct S var) map(mapper(default), tofrom: var)
#pragma omp declare mapper(only_x : struct S var) map(mapper(default), tofrom: var.x)

void f(){
  struct S z = {1,2};
#pragma omp target defaultmap(alloc)
  z.x += 5;
#pragma omp target map(z)
  z.x += 7;
#pragma omp target map(mapper(default), tofrom: z)
  z.x += 8;
#pragma omp target map(mapper(only_x), tofrom: z)
  z.x += 9;
if (z.x != 1+5+7+8+9) __builtin_abort ();
}

/* { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(-2\\) thread_limit\\(0\\) defaultmap\\(alloc\\) map\\(tofrom:z \\\[len: 8\\\]\\)" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(-2\\) thread_limit\\(0\\) map\\(tofrom:z \\\[len: 8\\\]\\)" 2 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(-2\\) thread_limit\\(0\\) map\\(struct:z \\\[len: 1\\\]\\) map\\(tofrom:z.x \\\[len: 4\\\]\\)" 1 "gimple" } }  */

int main() {
  f();
}
