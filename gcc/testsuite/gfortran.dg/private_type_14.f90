! { dg-do compile }
!
! PR fortran/51378
!
! Allow constructor to nonprivate parent compoents,
! even if the extension specified PRIVATE for its own components
!
! Contributed by Reinhold Bader
!
module type_ext
  type :: vec
     real, dimension(3) :: comp
     integer :: len
  end type vec
  type, extends(vec) :: l_vec
     private
     character(len=20) :: label = '01234567890123456789'
  end type l_vec
end module type_ext
program test_ext
  use type_ext
  implicit none
  type(vec) :: o_vec, oo_vec
  type(l_vec) :: o_l_vec
  integer :: i
!
  o_vec = vec((/1.0, 2.0, 3.0/),3)
!  write(*,*) o_vec%comp, o_vec%len
  o_l_vec = l_vec(comp=(/1.0, 2.0, 3.0/),len=3)
! partial constr. not accepted by ifort 11.1, fixed in 12.0 (issue 562240)
!  write(*,*) o_l_vec%comp, o_l_vec%len
!  write(*,*) o_l_vec%vec
  oo_vec = o_l_vec%vec
  do i=1, 3
    if (abs(oo_vec%comp(i) - o_vec%comp(i)) > 1.0E-5) then
       write(*, *) 'FAIL'
       stop
    end if
  end do
  write(*, *) 'OK'
end program
