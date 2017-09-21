! { dg-do run }
!
! Test the fix for PR52832
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
  subroutine testSub()
    interface
      integer function fcn1 (arg)
        integer :: arg
      end function
      integer function fcn2 (arg)
        integer :: arg
      end function
    end interface

    procedure(fcn1), pointer :: r
    r => fcn2
    associate (k => r)
      if (r(42) .ne. 84) call abort
    end associate
    r => fcn1
    associate (k => r)
      if (r(42) .ne. 42) call abort
    end associate
  end subroutine testSub

  integer function fcn1 (arg)
    integer :: arg;
    fcn2 = arg
  end function

  integer function fcn2 (arg)
    integer :: arg;
    fcn2 = arg*2
  end function

  call testSub
end
