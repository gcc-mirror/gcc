/* Exercise *_parser_oacc_shape_clause by checking various combinations
   of gang, worker and vector clause arguments.  */

/* { dg-compile } */

int main ()
{
  int i;
  int v = 32, w = 19;
  int length = 1, num = 5;
  int *abc;

  /* Valid uses.  */

  #pragma acc kernels
  #pragma acc loop gang worker vector
  for (i = 0; i < 10; i++)
    ;
  
  #pragma acc kernels
  #pragma acc loop gang(26)
  for (i = 0; i < 10; i++)
    ;
  
  #pragma acc kernels
  #pragma acc loop gang(v)
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop vector(length: 16)
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop vector(length: v)
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop vector(16)
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop vector(v)
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop worker(num: 16)
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop worker(num: v)
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop worker(16)
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop worker(v)
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(static: 16, num: 5)
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(static: v, num: w)
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop vector(length)
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop worker(num)
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(num, static: 6)
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(static: 5, num)
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(1, static:*)
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(static:*, 1)
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(1, static:*)
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(num: 5, static: 4)
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(num: v, static: w)
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(num, static:num)
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop vector(length:length)
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop worker(num:length)
  for (i = 0; i < 10; i++)
    ;  

  #pragma acc kernels
  #pragma acc loop worker(num:num)
  for (i = 0; i < 10; i++)
    ;  

  /* Invalid uses.  */
  
  #pragma acc kernels
  #pragma acc loop gang(16, 24) /* { dg-error "unexpected argument" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(v, w) /* { dg-error "unexpected argument" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(num: 1, num:2, num:3, 4) /* { dg-error "unexpected argument" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(num: 1 num:2, num:3, 4) /* { dg-error "expected '.' before" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(1, num:2, num:3, 4) /* { dg-error "unexpected argument" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(num, num:5) /* { dg-error "unexpected argument" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(length:num) /* { dg-error "" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop vector(5, length:length) /* { dg-error "expected '.' before" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop vector(num:length) /* { dg-error "" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop worker(length:5) /* { dg-error "expected '.' before" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop worker(1, num:2) /* { dg-error "expected '.' before" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(static: * abc)
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(static:*num:1) /* { dg-error "" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(num: 5 static: *) /* { dg-error "expected '.' before" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(,static: *) /* { dg-error "" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop vector(,length:5) /* { dg-error "" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop worker(,num:10) /* { dg-error "" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop worker(,10) /* { dg-error "" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop vector(,10) /* { dg-error "" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(,10) /* { dg-error "" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(-12) /* { dg-warning "" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(-1.0) /* { dg-error "" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(1.0) /* { dg-error "" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(num:-1.0) /* { dg-error "" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(num:1.0) /* { dg-error "" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(static:-1.0) /* { dg-error "" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop gang(static:1.0) /* { dg-error "" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop worker(-1.0) /* { dg-error "" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop worker(1.0) /* { dg-error "" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop worker(num:-1.0) /* { dg-error "" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop worker(num:1.0) /* { dg-error "" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop vector(-1.0) /* { dg-error "" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop vector(1.0) /* { dg-error "" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop vector(length:-1.0) /* { dg-error "" } */
  for (i = 0; i < 10; i++)
    ;

  #pragma acc kernels
  #pragma acc loop vector(length:1.0) /* { dg-error "" } */
  for (i = 0; i < 10; i++)
    ;

  return 0;
}
