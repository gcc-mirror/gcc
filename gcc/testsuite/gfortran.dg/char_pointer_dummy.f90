! { dg-do run }
! { dg-options "-std=legacy" }
!
program char_pointer_dummy
! Test character pointer dummy arguments, required
! to fix PR16939 and PR18689
! Provided by Paul Thomas pault@gcc.gnu.org
  implicit none
  character*4                :: c0
  character*4, pointer       :: c1
  character*4, pointer       :: c2(:)
  allocate (c1, c2(1))
! Check that we have not broken non-pointer characters.
  c0 = "wxyz"
  call foo (c0)
! Now the pointers
  c1 = "wxyz"
  call sfoo (c1)
  c2 = "wxyz"
  call afoo (c2)
  deallocate (c1, c2)
contains
  subroutine foo (cc1)
    character*4                :: cc1
    if (cc1 /= "wxyz") call abort ()
  end subroutine foo
  subroutine sfoo (sc1)
    character*4, pointer       :: sc1
    if (sc1 /= "wxyz") call abort ()
  end subroutine sfoo
  subroutine afoo (ac1)
    character*4, pointer       :: ac1(:)
    if (ac1(1) /= "wxyz") call abort ()
  end subroutine afoo
end program char_pointer_dummy

