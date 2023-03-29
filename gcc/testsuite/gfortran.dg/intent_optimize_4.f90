! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }
! { dg-final { scan-tree-dump-times "CLOBBER" 2 "original" } }
!
! PR fortran/106817
! Check that for an actual argument whose dummy is INTENT(OUT),
! the clobber that is emitted in the caller before a procedure call
! happens after any expression depending on the argument value has been
! evaluated.
! 

module m
  implicit none
contains
  subroutine copy1(out, in)
    integer, intent(in) :: in
    integer, intent(out) :: out
    out = in
  end subroutine copy1
  subroutine copy2(in, out)
    integer, intent(in) :: in
    integer, intent(out) :: out
    out = in
  end subroutine copy2
end module m

program p
  use m
  implicit none
  integer :: a, b

  ! Clobbering of a should happen after a+1 has been evaluated.
  a = 3
  call copy1(a, a+1)
  if (a /= 4) stop 1

  ! Clobbering order does not depend on the order of arguments.
  ! It should also come last with reversed arguments.
  b = 12
  call copy2(b+1, b)
  if (b /= 13) stop 2

end program p
