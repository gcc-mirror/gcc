#pragma omp declare target
#pragma acc routine seq /* { dg-error "cannot apply '#pragma acc routine' to '\(void \)?f1\(\\(\\)\)?', which has also been marked with an OpenMP 'declare target' directive" } */
void f1 (void) {}
#pragma omp end declare target

#pragma omp declare target
void f1 (void);

#pragma acc routine seq /* { dg-error "cannot apply '#pragma acc routine' to '\(void \)?f1\(\\(\\)\)?', which has also been marked with an OpenMP 'declare target' directive" } */
void f1 (void);



#pragma omp declare target
#pragma acc routine /* { dg-error "cannot apply '#pragma acc routine' to '\(void \)?f2\(\\(\\)\)?', which has also been marked with an OpenMP 'declare target' directive" } */
extern void f2 (void);
#pragma omp end declare target

#pragma omp declare target
extern void f2 (void);
#pragma omp end declare target

#pragma acc routine gang /* { dg-error "cannot apply '#pragma acc routine' to '\(void \)?f2\(\\(\\)\)?', which has also been marked with an OpenMP 'declare target' directive" } */
extern void f2 (void);


#pragma omp declare target
#pragma acc routine gang /* { dg-error "cannot apply '#pragma acc routine' to '\(void \)?f3\(\\(\\)\)?', which has also been marked with an OpenMP 'declare target' directive" } */
void f3 (void);
#pragma omp end declare target

#pragma omp declare target
void f3 (void) {}
#pragma omp end declare target

#pragma acc routine (f3) gang /* { dg-error "cannot apply '#pragma acc routine' to '\(void \)?f3\(\\(\\)\)?', which has also been marked with an OpenMP 'declare target' directive" } */


/* Surprisingly, this diagnosis also works for '#pragma acc routine' first,
   followed by '#pragma omp declare target'; the latter gets applied first.  */


#pragma acc routine /* { dg-error "cannot apply '#pragma acc routine' to '\(void \)?f4\(\\(\\)\)?', which has also been marked with an OpenMP 'declare target' directive" } */
extern void f4 (void);

#pragma omp declare target
extern void f4 (void);
#pragma omp end declare target


#pragma acc routine gang /* { dg-error "cannot apply '#pragma acc routine' to '\(void \)?f5\(\\(\\)\)?', which has also been marked with an OpenMP 'declare target' directive" } */
void f5 (void) {}

#pragma omp declare target
extern void f5 (void);
#pragma omp end declare target
