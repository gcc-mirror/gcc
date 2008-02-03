! { dg-do compile }
! PR fortran/32936
!
!
function all_res()
  implicit none
  real, pointer :: gain 
  integer :: all_res
  allocate (gain,STAT=all_res)
  deallocate(gain)
  call bar()
contains
  subroutine bar()
    real, pointer :: gain2
    allocate (gain2,STAT=all_res)
    deallocate(gain2)
  end subroutine bar
end function all_res

function func()
  implicit none
  real, pointer :: gain 
  integer :: all_res2, func
  func = 0
entry all_res2
  allocate (gain,STAT=all_res2)
  deallocate(gain)
contains
  subroutine test
    implicit none
    real, pointer :: gain2
     allocate (gain2,STAT=all_res2)
     deallocate(gain2)
  end subroutine test
end function func

function func2() result(res)
  implicit none
  real, pointer :: gain 
  integer :: res
  allocate (gain,STAT=func2) ! { dg-error "is not a variable" }
  deallocate(gain)
  res = 0
end function func2

subroutine sub()
  implicit none
  interface
    integer function func2()
    end function
  end interface
  real, pointer :: gain 
  integer, parameter :: res = 2
  allocate (gain,STAT=func2) ! { dg-error "STAT tag in ALLOCATE statement at .1. must be a variable" }
  deallocate(gain)
end subroutine sub

module test
contains
 function one()
   integer :: one, two
   integer, pointer :: ptr
   allocate(ptr, stat=one)
   if(one == 0) deallocate(ptr)
 entry two
   allocate(ptr, stat=two)
   if(associated(ptr)) deallocate(ptr)
 end function one
 subroutine sub()
   integer, pointer :: p
   allocate(p, stat=one) ! { dg-error "STAT tag in ALLOCATE statement at .1. must be a variable" }
   if(associated(p)) deallocate(p)
   allocate(p, stat=two) ! { dg-error "STAT tag in ALLOCATE statement at .1. must be a variable" }
   if(associated(p)) deallocate(p)
 end subroutine sub
end module test
