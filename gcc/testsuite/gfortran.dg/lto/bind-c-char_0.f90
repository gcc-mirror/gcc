! { dg-lto-do link }
! { dg-lto-options {{ -O0 -flto }} }
!
! PR fortran/102885

module m
  use iso_c_binding, only: c_char
  implicit none (type, external)

contains

! Assumed-shape array, nonallocatable/nonpointer

subroutine ar3 (xn, n) bind(C)
  integer :: n
  character(len=n) :: xn(..)
  if (size(xn) /= 6) stop
  if (len(xn) /= 5) stop  
  select rank(xn)
    rank(1)
      xn = ['FDGhf', &
            'hdrhg', &
            'fDgFl', &
            'DFHs3', &
            '4a54G', &
            'hSs6k']
  rank default
    stop
  end select
end

end

program main
  use m
  implicit none (type, external)
  character(kind=c_char, len=5) :: str5a6(6)

  ! assumed rank - with array descriptor

  str5a6 = ['DDGhf', &
            'hdrh$', &
            'fDGSl', &
            'DFHs3', &
            '43grG', &
            'hFG$k']
  call ar3 (str5a6, 5)

end
