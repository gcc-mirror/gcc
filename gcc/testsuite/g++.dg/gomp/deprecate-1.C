/* { dg-additional-options "-fdiagnostics-show-caret -Wdeprecated-openmp" } */

#pragma omp declare simd linear (val(a) : 1)	/* { dg-warning "specifying the list items as arguments to the modifiers is deprecated since OpenMP 5.2" } */
/* { dg-begin-multiline-output "" }
 #pragma omp declare simd linear (val(a) : 1)
                                  ^~~
                                  ---- -   -
                                           val, step (1)
   { dg-end-multiline-output "" } */
#pragma omp declare simd linear (val(b))	/* { dg-warning "specifying the list items as arguments to the modifiers is deprecated since OpenMP 5.2" } */
/* { dg-begin-multiline-output "" }
 #pragma omp declare simd linear (val(b))
                                  ^~~
                                  ---- -
                                        : val
   { dg-end-multiline-output "" } */
int foo (int a, int b);

#pragma omp declare simd linear (ref(a) : 1)	/* { dg-warning "specifying the list items as arguments to the modifiers is deprecated since OpenMP 5.2" } */
/* { dg-begin-multiline-output "" }
 #pragma omp declare simd linear (ref(a) : 1)
                                  ^~~
                                  ---- -   -
                                           ref, step (1)
   { dg-end-multiline-output "" } */
#pragma omp declare simd linear (ref(b))	/* { dg-warning "specifying the list items as arguments to the modifiers is deprecated since OpenMP 5.2" } */
/* { dg-begin-multiline-output "" }
 #pragma omp declare simd linear (ref(b))
                                  ^~~
                                  ---- -
                                        : ref
   { dg-end-multiline-output "" } */
int baz (int &a, int &b);

#pragma omp declare simd linear (uval(a) : 1)	/* { dg-warning "specifying the list items as arguments to the modifiers is deprecated since OpenMP 5.2" } */
/* { dg-begin-multiline-output "" }
 #pragma omp declare simd linear (uval(a) : 1)
                                  ^~~~
                                  ----- -   -
                                            uval, step (1)
   { dg-end-multiline-output "" } */
#pragma omp declare simd linear (uval(b))	/* { dg-warning "specifying the list items as arguments to the modifiers is deprecated since OpenMP 5.2" } */
/* { dg-begin-multiline-output "" }
 #pragma omp declare simd linear (uval(b))
                                  ^~~~
                                  ----- -
                                         : uval
   { dg-end-multiline-output "" } */
int qux (int &a, int &b);

int v;
#pragma omp declare target to (v)		/* { dg-warning "'to' clause with 'declare target' deprecated since OpenMP 5.2, use 'enter'" } */
/* { dg-begin-multiline-output "" }
 #pragma omp declare target to (v)
                            ^~
                            enter
   { dg-end-multiline-output "" } */

void
bar ()
{
  int r = 0;
  #pragma omp parallel reduction (-:r)		/* { dg-warning "'-' operator for reductions deprecated in OpenMP 5.2" } */
/* { dg-begin-multiline-output "" }
   #pragma omp parallel reduction (-:r)
                                   ^
                                   +
   { dg-end-multiline-output "" } */
  ++r;
  #pragma omp for ordered (1)
  for (int i = 0; i < 64; i++)
    {
      #pragma omp ordered depend (sink: i - 1)	/* { dg-warning "'sink' modifier with 'depend' clause deprecated since OpenMP 5.2, use with 'doacross'" } */
/* { dg-begin-multiline-output "" }
       #pragma omp ordered depend (sink: i - 1)
                           ^~~~~~
                           doacross
   { dg-end-multiline-output "" } */
      #pragma omp ordered depend (source)	/* { dg-warning "'source' modifier with 'depend' clause deprecated since OpenMP 5.2, use with 'doacross'" } */
/* { dg-begin-multiline-output "" }
       #pragma omp ordered depend (source)
                           ^~~~~~
                           doacross
   { dg-end-multiline-output "" } */
    }
  #pragma omp target map (always close present, tofrom: r)	/* { dg-warning "'map' clause modifiers without comma separation is deprecated since OpenMP 5.2" } */
/* { dg-begin-multiline-output "" }
   #pragma omp target map (always close present, tofrom: r)
                          ^
                                  ,
   #pragma omp target map (always close present, tofrom: r)
                          ^
                                        ,
   { dg-end-multiline-output "" } */
  ;
  #pragma omp parallel proc_bind (master)	/* { dg-warning "'master' affinity deprecated since OpenMP 5.1, use 'primary'" } */
/* { dg-begin-multiline-output "" }
   #pragma omp parallel proc_bind (master)
                        ^~~~~~~~~
                                   ------
                                   primary
   { dg-end-multiline-output "" } */
  ;
  #pragma omp parallel master			/* { dg-warning "'master' construct deprecated since OpenMP 5.1, use 'masked'" } */
/* { dg-begin-multiline-output "" }
   #pragma omp parallel master
                        ^~~~~~
                        masked
   { dg-end-multiline-output "" } */
  ;
  #pragma omp master				/* { dg-warning "'master' construct deprecated since OpenMP 5.1, use 'masked'" } */
/* { dg-begin-multiline-output "" }
   #pragma omp master
                                                                                                                                    ^
   { dg-end-multiline-output "" } */
  ;
  #pragma omp metadirective when (device={arch("blahblah")}: nothing) default (nothing)		/* { dg-warning "'default' clause on metadirectives deprecated since OpenMP 5.2, use 'otherwise'" } */
/* { dg-begin-multiline-output "" }
   #pragma omp metadirective when (device={arch("blahblah")}: nothing) default (nothing)
                                                                       ^~~~~~~
                                                                       otherwise
   { dg-end-multiline-output "" } */
  ;
}
