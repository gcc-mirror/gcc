! { dg-do run }
! Tests the fix for PR31213, which exposed rather a lot of
! bugs - see the PR and the ChangeLog.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
module mykinds
  implicit none
  integer, parameter :: ik1 = selected_int_kind (2)
  integer, parameter :: ik2 = selected_int_kind (4)
  integer, parameter :: dp = selected_real_kind (15,300)
end module mykinds

module spec_xpr
  use mykinds
  implicit none
  integer(ik2) c_size
contains
  pure function tricky (str,ugly)
    character(*), intent(in) :: str
    integer(ik1) :: ia_ik1(len(str))
    interface yoagly
      pure function ugly(n)
        use mykinds
        implicit none
        integer, intent(in) :: n
        complex(dp) :: ugly(3*n+2)
      end function ugly
    end interface yoagly
    logical :: la(size (yoagly (size (ia_ik1))))
    integer :: i
    character(tricky_helper ((/(.TRUE., i=1, size (la))/)) + c_size) :: tricky

    tricky = transfer (yoagly (1), tricky)
  end function tricky

  pure function tricky_helper (lb)
    logical, intent(in) :: lb(:)
    integer :: tricky_helper
    tricky_helper = 2 * size (lb) + 3
  end function tricky_helper
end module spec_xpr

module xtra_fun
  implicit none
contains
  pure function butt_ugly(n)
    use mykinds
    implicit none
    integer, intent(in) :: n
    complex(dp) :: butt_ugly(3*n+2)
    real(dp) pi, sq2

    pi = 4 * atan (1.0_dp)
    sq2 = sqrt (2.0_dp)
    butt_ugly = cmplx (pi, sq2, dp)
  end function butt_ugly
end module xtra_fun

program spec_test
  use mykinds
  use spec_xpr
  use xtra_fun
  implicit none
  character(54) :: chr

  c_size = 5
  if (tricky ('Help me', butt_ugly) .ne. transfer (butt_ugly (1), chr)) call abort ()
end program spec_test
