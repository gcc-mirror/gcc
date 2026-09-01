! PR tree-optimization/127092
! { dg-do run }


program main
  implicit none
  if (transfer_count(3, 8) /= 1) error stop 1
contains
  integer function transfer_count(n, m)
    integer, intent(in) :: n, m
    character(len=n) :: source
    character(len=m) :: mold(1)
    if (len(source, kind=8) >= len(mold, kind=8)) then
      transfer_count = -1
      return
    end if
    source = ""
    mold = ""
    transfer_count = size(transfer(source, mold))
  end function
end program
