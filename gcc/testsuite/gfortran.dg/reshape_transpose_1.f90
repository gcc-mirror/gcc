! { dg-do run }
! PR 31196 - reshape of transposed derived types generated
!            wront results.
program main
  implicit none
  TYPE datatype
     INTEGER :: I
  END TYPE datatype
  character (len=20) line1, line2
  TYPE(datatype), dimension(2,2) :: data, result
  data(1,1)%i = 1
  data(2,1)%i = 2
  data(1,2)%i = 3
  data(2,2)%i = 4
  write (unit=line1, fmt="(4I4)") reshape(transpose(data),shape(data))
  write (unit=line2, fmt="(4I4)") (/ 1, 3, 2, 4 /)
  if (line1 /= line2) call abort
END program main
