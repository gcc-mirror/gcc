! { dg-do run }
!
! Tests fix for PR82184
!
! Contributed by Andrey Guskov  <andrey.y.guskov@intel.com)
!
program r187
  call s()
  call s()
contains
  subroutine s()
    complex(4), allocatable, save :: a(:, :)
    complex(4), pointer,     save :: b(:, :)
    if (.not. associated(b)) then
      allocate(a(2, 2))
      allocate(b(2, 2))
      a = reshape ([cmplx(1, 1), cmplx(2, 2), cmplx(1, 2), cmplx(2, 1)], [2,2])
    else
      b = transpose(a)
      if (merge("PASSED", "FAILED", all (transpose (a) .eq. b)) .eq. "FAILED") STOP 1
    end if
  end subroutine s
end program r187
