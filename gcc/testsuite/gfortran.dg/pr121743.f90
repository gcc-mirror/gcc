! PR fortran/121743
! { dg-do compile }
! { dg-options "-fcoarray=lib" }

program pr121743
end program pr121743
subroutine foo ()
  integer, allocatable, dimension(:), codimension[:] :: s
  integer :: i
  i = s(1)[1]
end
