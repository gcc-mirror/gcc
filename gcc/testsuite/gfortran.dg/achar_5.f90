! { dg-do compile }
!
program test

  print *, char(255)
  print *, achar(255)
  print *, char(255,kind=1)
  print *, achar(255,kind=1)
  print *, char(255,kind=4)
  print *, achar(255,kind=4)

  print *, char(0)
  print *, achar(0)
  print *, char(0,kind=1)
  print *, achar(0,kind=1)
  print *, char(0,kind=4)
  print *, achar(0,kind=4)

  print *, char(297) ! { dg-error "too large for the collating sequence" }
  print *, achar(297) ! { dg-error "too large for the collating sequence" }
  print *, char(297,kind=1) ! { dg-error "too large for the collating sequence" }
  print *, achar(297,kind=1) ! { dg-error "too large for the collating sequence" }
  print *, char(297,kind=4)
  print *, achar(297,kind=4)

  print *, char(-1) ! { dg-error "negative" }
  print *, achar(-1) ! { dg-error "negative" }
  print *, char(-1,kind=1) ! { dg-error "negative" }
  print *, achar(-1,kind=1) ! { dg-error "negative" }
  print *, char(-1,kind=4) ! { dg-error "negative" }
  print *, achar(-1,kind=4) ! { dg-error "negative" }

  print *, char(huge(0_8)) ! { dg-error "too large for the collating sequence" }
  print *, achar(huge(0_8)) ! { dg-error "too large for the collating sequence" }
  print *, char(huge(0_8),kind=1) ! { dg-error "too large for the collating sequence" }
  print *, achar(huge(0_8),kind=1) ! { dg-error "too large for the collating sequence" }
  print *, char(huge(0_8),kind=4) ! { dg-error "too large for the collating sequence" }
  print *, achar(huge(0_8),kind=4) ! { dg-error "too large for the collating sequence" }

end program test
