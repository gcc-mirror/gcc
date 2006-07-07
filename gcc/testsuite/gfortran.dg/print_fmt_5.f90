! { dg-do compile }
! print_fmt_5.f90
! Test of fix for PR28237 and the last bit of PR23420.  See
! below for the description of the problem.
!
program r
  character(12) :: for = '(i5)', left = '(i', right = ')'
  integer :: i, j
  integer :: h(4) &
    = (/1h(, 1hi, 1h5, 1h)/)! { dg-warning "HOLLERITH|Hollerith" }
  namelist /mynml/ i
  i = fact ()
!
! All these are "legal" things to do; note however the warnings
! for extensions or obsolete features!
!
  print *, fact()
  print 100, fact()
  print '(i5)', fact()
  print mynml      ! { dg-warning "is an extension" }
  do i = 1, 5
    print trim(left)//char(iachar('0') + i)//trim(right), i
  end do
  assign 100 to i  ! { dg-warning "ASSIGN statement" }
  print i, fact()  ! { dg-warning "ASSIGNED variable" }
  print h, fact () ! { dg-warning "Non-character in FORMAT" }
!
! These are not and caused a segfault in trans-io:560
!
! PR28237
  print fact()     ! { dg-error "not an ASSIGNED variable" }
! original PR23420
  print precision(1.2_8) ! { dg-error "type default CHARACTER" }
! PR23420 points 4 and 5
  print j + j      ! { dg-error "not an ASSIGNED variable" }
! An extension of the above, encountered in writing the fix
  write (*, fact())! { dg-error "not an ASSIGNED variable" }
 100 format (i5)
contains
  function fact()
    integer :: fact
    fact = 1
  end function fact
end

