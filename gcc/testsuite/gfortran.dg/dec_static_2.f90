! { dg-do run }
! { dg-options "-fdec-static -fno-automatic -finit-local-zero" }
!
! Test STATIC and AUTOMATIC with -fno-automatic and recursive subroutines.
!
subroutine assert(s, i1, i2)
  implicit none
  integer, intent(in)      :: i1, i2
  character(*), intent(in) :: s
  if (i1 .ne. i2) then
    print *, s, ": expected ", i2, " but was ", i1
    call abort
  endif
endsubroutine

function f (x)
implicit none
  integer f
  integer, intent(in) :: x
  integer, static     :: a ! should be SAVEd
  a = a + x ! should increment by x every time
  f = a
  return
endfunction

recursive subroutine g (x)
implicit none
  integer, intent(in) :: x
  integer, automatic  :: a ! should be automatic (in recursive)
  a = a + x ! should be set to x every time
  call assert ("g%a", a, x)
endsubroutine

subroutine h (x)
implicit none
  integer, intent(in) :: x
  integer, automatic  :: a ! should be automatic (outside recursive)
  a = a + x ! should be set to x every time
  call assert ("h%a", a, x)
endsubroutine

implicit none
integer :: f

! Should return static value of c; accumulates x
call assert ("f()", f(3), 3)
call assert ("f()", f(4), 7)
call assert ("f()", f(2), 9)

call g(3)
call g(4)
call g(2)

call h(3)
call h(4)
call h(2)

end
