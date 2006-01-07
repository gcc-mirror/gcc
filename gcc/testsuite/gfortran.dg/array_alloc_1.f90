! PR 21104.  Make sure that either f() or its caller will allocate
! the array data.  We've decided to make the caller allocate it.
! { dg-do run }
program main
  implicit none
  call test (f ())
contains
  subroutine test (x)
    integer, dimension (10) :: x
    integer :: i
    do i = 1, 10
      if (x (i) .ne. i * 100) call abort
    end do
  end subroutine test

  function f ()
    integer, dimension (10) :: f
    integer :: i
    forall (i = 1:10) f (i) = i * 100
  end function f
end program main
