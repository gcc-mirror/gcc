! { dg-do compile }
! { dg-options "-std=f2003" }
!
! Test our conformance to item 4.9 ("Kind type parameters of integer
! specifiers") of the Fortran 2003 status document at
! ftp://ftp.nag.co.uk/sc22wg5/N1551-N1600/N1579.pdf
!
! The non-default integer variables are allowed since Fortran 2003.
! The non-default logical variables are allowed since Fortran 2008.

  integer(kind=8) :: i, j, k, n
  logical(kind=8) :: l1, l2, l3

  open(10, status="scratch", iostat=i)

  backspace(10, iostat=i)
  endfile(10, iostat=i)
  rewind(10, iostat=i)

  read(*, '(I2)', iostat=i) k
  read(*, '(I2)', advance='no', size=j) k

  inquire(iolength=i) "42"
  inquire(10, iostat=i)
  inquire(10, number=j)
  inquire(10, recl=k)
  inquire(10, nextrec=n)

  inquire(10, exist=l1) ! { dg-error "Non-default LOGICAL kind" }
  inquire(10, named=l3) ! { dg-error "Non-default LOGICAL kind" }
  inquire(10, opened=l2) ! { dg-error "Non-default LOGICAL kind" }
  inquire(10, pending=l2) ! { dg-error "Non-default LOGICAL kind" }

  close(10, iostat=i)

end
