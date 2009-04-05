! { dg-do "run" }
! PR fortran/29458 - spurious warning for implied do-loop counter

  integer :: n, i
  i = 10
  n = 5
  n = SUM((/(i,i=1,n)/))

  ! 'i' must not be changed
  IF (i /= 10) CALL abort()
END
