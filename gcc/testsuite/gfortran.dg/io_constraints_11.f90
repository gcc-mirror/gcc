! { dg-do compile }
! { dg-options "-std=f95" }
!
! Test our conformance to item 4.9 ("Kind type parameters of integer
! specifiers") of the Fortran 2003 status document at
! ftp://ftp.nag.co.uk/sc22wg5/N1551-N1600/N1579.pdf
!
! The non-default integer variables are allowed since Fortran 2003.
! The non-default logical variables are allowed since Fortran 2008.

  integer(kind=8) :: i, j, k, n
  logical(kind=8) :: l1, l2, l3

  open(10, status="scratch", iostat=i) ! { dg-error "requires default INTEGER" }

  backspace(10, iostat=i) ! { dg-error "requires default INTEGER" }
  endfile(10, iostat=i) ! { dg-error "requires default INTEGER" }
  rewind(10, iostat=i) ! { dg-error "requires default INTEGER" }

  read(*, '(I2)', iostat=i) k ! { dg-error "requires default INTEGER" }
  read(*, '(I2)', advance='no', size=j) k ! { dg-error "requires default INTEGER" }

  inquire(iolength=i) "42" ! { dg-error "requires default INTEGER" }
  inquire(10, iostat=i) ! { dg-error "requires default INTEGER" }
  inquire(10, number=j) ! { dg-error "requires default INTEGER" }
  inquire(10, recl=k) ! { dg-error "requires default INTEGER" }
  inquire(10, nextrec=n) ! { dg-error "requires default INTEGER" }

  inquire(10, exist=l1) ! { dg-error "Non-default LOGICAL kind" }
  inquire(10, named=l3) ! { dg-error "Non-default LOGICAL kind" }
  inquire(10, opened=l2) ! { dg-error "Non-default LOGICAL kind" }
  inquire(10, pending=l2) ! { dg-error "Non-default LOGICAL kind" }

  close(10, iostat=i) ! { dg-error "requires default INTEGER" }

end
