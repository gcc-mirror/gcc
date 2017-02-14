! { dg-do run }

  implicit none
  integer i
  character(LEN=6) :: a(1) = "123456"
  forall (i = 3:4) a(1)(i:i+2) = a(1)(i-2:i)
  !print *,a ! displays '12@' must be '121234'
  IF (a(1) .ne. "121234") call abort
end
