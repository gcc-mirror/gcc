void
f (int i, int j, int k)
{
#pragma acc kernels
#pragma acc loop gang
  for (i = 0; i < 20; ++i)
    ;

#pragma acc kernels
#pragma acc loop gang (num: 10)
  for (i = 0; i < 20; ++i)
    ;

#pragma acc kernels
#pragma acc loop gang (static: 10)
  for (i = 0; i < 20; ++i)
    ;

#pragma acc kernels
#pragma acc loop gang (static: 5, num: 10)
  for (i = 0; i < 20; ++i)
    ;


#pragma acc kernels
#pragma acc loop gang (static: 5, num: 10, *) /* { dg-error "duplicate operand to clause" } */
  for (i = 0; i < 20; ++i)
    ;

#pragma acc kernels
#pragma acc loop gang (static: 5, num: 10, static: *) /* { dg-error "duplicate 'num' argument" } */
  for (i = 0; i < 20; ++i)
    ;

#pragma acc kernels
#pragma acc loop worker (static: 234) /* { dg-error "expected 'num' before" } */
  for (i = 0; i < 20; ++i)
    ;

#pragma acc kernels
#pragma acc loop worker (num: 234)
  for (i = 0; i < 20; ++i)
    ;

#pragma acc kernels
#pragma acc loop worker (num: 234, num: 12) /* { dg-error "duplicate operand to clause" } */
  for (i = 0; i < 20; ++i)
    ;

#pragma acc kernels
#pragma acc loop vector /* { dg-error "gang, worker and vector must occur in this order in a loop nest" } */
  for (i = 0; i < 20; ++i)
#pragma acc loop worker
    for (j = 0; j < 25; ++j)
      ;

#pragma acc kernels
#pragma acc loop worker (length: 20) /* { dg-error "expected 'num' before 'length'" } */
  for (i = 0; i < 20; ++i)
#pragma acc loop vector (length: 10)
    for (j = 0; j < 25; ++j)
      ;

#pragma acc kernels
#pragma acc loop worker
  for (i = 0; i < 20; ++i)
#pragma acc loop vector
    for (j = 0; j < 25; ++j)
      ;
}
