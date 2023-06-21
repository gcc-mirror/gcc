! { dg-do run }
! Test fixes for PR109451
! Contributed by Harald Anlauf  <anlauf@gcc.gnu.org>
!
program p
   implicit none
   character(4) :: c(2) = ["abcd","efgh"]
   call dcs3 (c)
   call dcs0 (c)
contains
  subroutine dcs3 (a)
    character(len=*), intent(in)  :: a(:)
    character(:),     allocatable :: b(:)
    b = a(:)
    call test (b, a, 1)
    associate (q => b(:))    ! no ICE but print repeated first element
      call test (q, a, 2)
      print *, q             ! Checked with dg-output
      q = q(:)(2:3)
    end associate
    call test (b, ["bc  ","fg  "], 4)
    b = a(:)
    associate (q => b(:)(:)) ! ICE
      call test (q, a, 3)
      associate (r => q(:)(1:3))
        call test (r, a(:)(1:3), 5)
      end associate
    end associate
    associate (q => b(:)(2:3))
      call test (q, a(:)(2:3), 6)
    end associate
  end subroutine dcs3

! The associate vars in dsc0 had string length not set
  subroutine dcs0 (a)
    character(len=*), intent(in) :: a(:)
    associate (q => a)
      call test (q, a, 7)
    end associate
    associate (q => a(:))
      call test (q, a, 8)
    end associate
    associate (q => a(:)(:))
      call test (q, a, 9)
    end associate
  end subroutine dcs0

  subroutine test (x, y, i)
    character(len=*), intent(in) :: x(:), y(:)
    integer, intent(in) :: i
    if (any (x .ne. y)) stop i
  end subroutine test
end program p
! { dg-output " abcdefgh" }
