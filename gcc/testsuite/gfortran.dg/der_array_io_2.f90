! Test IO of arrays in derived type arrays
! { dg-do run }
program main

  character *1000 buf1, buf2

  type :: foo_type                                             
     integer x(3)
     integer y(4)
     integer z(5)
     character*11 a(3)
  end type foo_type
                                                                        
  type (foo_type) :: foo(2)
  
  foo(1)%x = 3
  foo(1)%y = 4
  foo(1)%z = 5
  foo(1)%a = "hello world"

  foo(2)%x = 30
  foo(2)%y = 40
  foo(2)%z = 50
  foo(2)%a = "HELLO WORLD"

  print (buf1,*), foo
  print (buf2,*), ((foo(i)%x(j),j=1,3), (foo(i)%y(j),j=1,4), (foo(i)%z(j),j=1,5), (foo(i)%a(j),j=1,3), i=1,2)
  if (buf1.ne.buf2) call abort
end program main
