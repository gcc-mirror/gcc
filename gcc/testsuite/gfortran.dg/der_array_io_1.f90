! Test IO of arrays of integers in derived types
! { dg-do run }
program main

  character* 10000 :: buf1, buf2
  type xyz
     integer :: x, y(3), z
  end type xyz

  type (xyz) :: foo(4)

  do i=1,ubound(foo,1)
     foo(i)%x = 100*i
     do j=1,3
        foo(i)%y(j) = 100*i + 10*j 
     enddo
     foo(i)%z = 100*i+40
  enddo

  write (buf1, '(20i4)')  foo
  write (buf2, '(20i4)')  (foo(i)%x, (foo(i)%y(j), j=1,3), foo(i)%z, i=1,4)

  if (buf1.ne.buf2) call abort
end program main
