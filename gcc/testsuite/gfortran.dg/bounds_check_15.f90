! { dg-do run }
! { dg-options "-fbounds-check" }
! Test the fix for PR42783, in which a bogus array bounds violation
! with missing optional array argument.
!
! Contributed by Harald Anlauf <anlauf@gmx.de>
!
program gfcbug99
  implicit none
  character(len=8), parameter :: mnem_list(2) = "A"

  call foo (mnem_list)  ! This call succeeds
  call foo ()           ! This call fails
contains
  subroutine foo (mnem_list)
    character(len=8) ,intent(in) ,optional :: mnem_list(:)

    integer            :: i,j
    character(len=256) :: ml
    ml = ''
    j = 0
    if (present (mnem_list)) then
       do i = 1, size (mnem_list)
          if (mnem_list(i) /= "") then
             j = j + 1
             if (j > len (ml)/8) call abort ()
             ml((j-1)*8+1:(j-1)*8+8) = mnem_list(i)
          end if
       end do
    end if
    if (j > 0) print *, trim (ml(1:8))
  end subroutine foo
end program gfcbug99
