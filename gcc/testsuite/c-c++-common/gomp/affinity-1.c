void
foo(int x)
{ 
  int a = 1;
  int b[5] = {1, 0, 1, 1, 0};
  int cc = 7;
  int d[5][5] = {{1, 2, 3, 4, 5},  {6, 7, 8, 9, 10},  {1, 2, 3, 4, 5},
		 {6, 7, 8, 9, 10}, {-1, -2, -3, -4,-5}};
#pragma omp taskgroup
 {
  #pragma omp task affinity(a)
    ;
  #pragma omp task affinity(iterator(i=(int)__builtin_cos(1.0+a):5, jj =2:5:2) : b[i], d[i][jj])
    ;
  #pragma omp task affinity(iterator(i=(int)__builtin_cos(1.0+a):5) : b[i], d[i][i])
    ;
  #pragma omp task affinity (iterator(i=1:5): a)
    ;
  #pragma omp task affinity (iterator(i=1:5): a) affinity(iterator(i=1:5) : x)
    ;
  #pragma omp task affinity (iterator(unsigned long j=1:5, k=7:4:-1) : b[j+k],a) affinity (cc)
    ;
 }
}
