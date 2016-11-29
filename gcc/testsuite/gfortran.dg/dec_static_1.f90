! { dg-do run }
! { dg-options "-fdec-static -finit-local-zero" }
!
! Test AUTOMATIC and STATIC attributes.
!
subroutine assert(s, i1, i2)
  implicit none
  integer, intent(in)      :: i1, i2
  character(*), intent(in) :: s
  if (i1 .ne. i2) then
    print *, s, ": expected ", i2, " but was ", i1
    call abort
  endif
endsubroutine assert

function f (x, y)
  implicit none
  integer f
  integer, intent(in)  :: x, y
  integer              :: a    ! only a can actually be saved
  integer, automatic   :: c    ! should actually be automatic
  save

  ! a should be incremented by x every time and saved
  a = a + x
  f = a

  ! c should be zeroed every time, therefore equal y
  c = c + y
  call assert ("f%c", c, y)
  return
endfunction

implicit none
integer :: f

! Should return static value of a; accumulates x
call assert ("f()", f(1,3), 1)
call assert ("f()", f(1,4), 2)
call assert ("f()", f(1,2), 3)

end
