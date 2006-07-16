! { dg-do run }
! PR 27980 - We used to allocate negative amounts of memory
!            for functions returning arrays if lbound > ubound-1.
!            Based on a test case by beliavsky@aol.com posted to
!            comp.lang.fortran.
program xint_func
  implicit none
  integer, parameter :: n=3,ii(n)=(/2,0,-1/)
  integer            :: i
  character(len=80)  :: line
  do i=1,n
     write (line,'(10I5)') int_func(ii(i))
  end do
contains
  function int_func(n) result(ivec)
    integer, intent(in) :: n
    integer             :: ivec(n)
    integer             :: i
    if (n > 0) then
       forall (i=1:n) ivec(i) = i
    end if
  end function int_func
end program xint_func
