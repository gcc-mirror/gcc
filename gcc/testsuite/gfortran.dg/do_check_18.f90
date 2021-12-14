! { dg-do compile }
! PR103718,
! PR103719 - ICE in doloop_contained_procedure_code
! Contributed by G.Steinmetz

subroutine s1
  integer :: i
  do i = 1, 2
     call s
  end do
contains
  subroutine s
    integer :: n
    inquire (iolength=n) 0  ! valid
  end
end

subroutine s2
  integer :: i
  do i = 1, 2
     call s
  end do
contains
  subroutine s
    shape(1) = 0    ! { dg-error "Non-variable expression" }
  end
end
