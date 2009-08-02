! Test IO of character arrays in derived types.
! { dg-do run }
! { dg-options "-std=legacy" }
!
program main
 character*1000 buf1, buf2
 type :: foo_type
     character(12), dimension(13) :: name = "hello world "
  end type foo_type
  type (foo_type) :: foo
!  foo = foo_type("hello world ")
  write (buf1,*)  foo
  write (buf2,*)  (foo%name(i), i=1,13)
  if (buf1.ne.buf2) call abort
end program main
