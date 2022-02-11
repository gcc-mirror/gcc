! PR libfortran/101255
! { dg-do run }

program test
  use ISO_FORTRAN_ENV, only: IOSTAT_EOR, IOSTAT_END
  implicit none
  character(len=50) :: err
  integer :: i

  err = ""
  flush(99, iostat=i, iomsg=err)

  if (err == "") stop 1
  if (i >= 0) stop 2
  if (i == IOSTAT_EOR .or. i == IOSTAT_END) stop 3
end
