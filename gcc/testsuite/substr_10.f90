! { dg-do compile }
! PR93340 - test error handling of substring simplification

subroutine p
  integer,parameter :: k = len ('a'(:0))
  integer,parameter :: m = len ('a'(0:)) ! { dg-error "Substring start index" }
  call foo ('bcd'(-8:-9))
  call foo ('bcd'(-9:-8)) ! { dg-error "Substring start index" }
  call foo ('bcd'(:12))   ! { dg-error "Substring end index" }
  call foo ('bcd'(-12:))  ! { dg-error "Substring start index" }
end
