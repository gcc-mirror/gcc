/* { dg-do compile } */
/* { dg-additional-options "-fopenmp -fdump-tree-gimple" } */

struct bar
{
  int num_vectors;
  double *vectors;
};

struct foo
{
  int num_vectors;
  struct bar *bars;
  double **vectors;
};

void func (struct foo *f, int n, int m)
{
  #pragma omp target enter data map (to: f->vectors[m][:n])
  #pragma omp target enter data map (to: f->bars[n].vectors[:m])
  #pragma omp target enter data map (to: f->bars[n].vectors[:f->bars[n].num_vectors])
}

/* { dg-final { scan-tree-dump-times {map\(struct:\*f \[len: 1\]\) map\(alloc:[a-z0-9\._]+->vectors \[len: 0\]\) map\(to:\*_[0-9]+ \[len: _[0-9]+\]\) map\(attach:[a-z0-9\._]+->vectors \[bias: [^\]]+\]\) map\(attach:\*_[0-9]+ \[bias: [^\]]+\]\)} 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times {map\(struct:\*\(f->bars \+ \(sizetype\) \(\([^\)]+\) n \* (?:16|8)\)\) \[len: 1\]\) map\(alloc:[a-z0-9\._]+->vectors \[len: 0\]\) map\(to:\*_[0-9]+ \[len: _[0-9]+\]\) map\(attach:[a-z0-9\._]+->vectors \[bias: [^\]]+\]\)} 2 "gimple" } } */
