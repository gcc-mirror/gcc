! { dg-do compile }
! PR fortran/64925
! Original test case provided by Bill Long <longb at cray dot com>
!
subroutine foo(nnn, aaa, bbb, ccc, ddd)
  implicit none
  integer :: nnn, aaa, bbb(nnn)
  integer :: i
  do i=1,nnn
     aaa = aaa + bbb(ccc(i))
  end do
  call ddd(aaa)
contains
  integer function ccc(i)  ! { dg-error "conflicts with DUMMY" }
    integer :: i
    ccc = i
  end function ccc
  subroutine ddd(j)        ! { dg-error "conflicts with DUMMY" }
    integer j
    j = j + 1 
  end subroutine ddd
end subroutine foo
