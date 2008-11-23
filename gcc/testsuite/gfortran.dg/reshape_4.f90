! { dg-do run }
! { dg-options "-fbounds-check" }
program main
  real, dimension(2,2) :: result
  real, dimension(6) :: source
  real, dimension(2) :: pad

  call random_number (source)
  call random_number (pad)

  result = reshape(source, shape(result),pad=pad(1:0))
  result = reshape(source, shape(result))
  result = reshape(source, shape(result),pad=pad)

end program main
