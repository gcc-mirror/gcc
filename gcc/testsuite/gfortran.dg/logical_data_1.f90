! { dg-do compile }
! PR19589
! Logical objects/values with differing type kinds were being rejected in
! data statements.
program logical_data_1
  logical(kind=4) :: a
  logical(kind=8) :: b
  data a, b /.true., .false./
end program
