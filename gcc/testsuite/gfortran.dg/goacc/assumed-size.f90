! Test if implicitly determined data clauses work with an
! assumed-sized array variable.  Note that the array variable, 'a',
! has been explicitly copied in and out via acc enter data and acc
! exit data, respectively.

! This does not appear to be supported by the OpenACC standard as of version
! 3.0.  There is however real-world code that relies on this working, so we
! make an attempt to support it.

program test
  implicit none

  integer, parameter :: n = 100
  integer a(n), i

  call dtest (a, n)

  do i = 1, n
     if (a(i) /= i) call abort
  end do
end program test

subroutine dtest (a, n)
  integer i, n
  integer a(*)

  !$acc enter data copyin(a(1:n))

  !$acc parallel loop
  do i = 1, n
     a(i) = i
  end do

  !$acc exit data copyout(a(1:n))
end subroutine dtest
