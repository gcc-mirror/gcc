! { dg-do run { target fd_truncate } }
! pr 18392
! test namelist with derived types
! Based on example provided by thomas.koenig@online.de

program pr18392
  implicit none
  type foo
     integer a
     real b
  end type foo
  type(foo) :: a
  namelist /nl/ a
  open (10, status="scratch")
  write (10,*) " &NL"
  write (10,*) " A%A = 10,"
  write (10,*) "/"
  rewind (10)
  read (10,nl)
  close (10)
  IF (a%a /= 10.0) call abort ()
end program pr18392
