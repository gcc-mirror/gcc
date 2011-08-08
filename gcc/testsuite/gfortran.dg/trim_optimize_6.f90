! { dg-do run }
! PR 47065 - make sure that impure functions are not evaluated twice when
! replacing calls to trim with expression(1:len_trim)
module foo
  implicit none
contains
  function f()
    integer :: f
    integer :: s=0
    s = s + 1
    f = s
  end function f
end module foo

program main
  use foo
  implicit none
  character(len=10) :: line
  character(len=4) :: b(2)
  b(1) = 'a'
  b(2) = 'bc'
  write(unit=line,fmt='(A,A)') trim(b(f())), "X"
  if (line /= "aX          ") call abort
  if (f() .ne. 2) call abort
end program main

! { dg-final { cleanup-modules "foo" } }
