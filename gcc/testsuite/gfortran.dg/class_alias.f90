! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! test for aliasing violations when converting class objects with
! different target and pointer attributes.
!
module test_module

  implicit none

  type, public :: test
    integer :: x
  end type test

contains

  subroutine do_it6 (par2_t)
    class (test), target :: par2_t
    par2_t%x = par2_t%x + 1
  end subroutine do_it6
   
  subroutine do_it5 (par1_p)
    class (test), pointer, intent(in) :: par1_p
    ! pointer -> target
    ! { dg-final { scan-tree-dump "par2_t\[^\n]*VIEW_CONVERT_EXPR\[^\n]*par1_p" "original" } }
    call do_it6 (par1_p)
  end subroutine do_it5

  subroutine do_it4 (par_p)
    class (test), pointer, intent(in) :: par_p
    ! pointer -> pointer
    ! { dg-final { scan-tree-dump-not "par1_p\[^\n]*VIEW_CONVERT_EXPR\[^\n]*par_p" "original" } }
    call do_it5 (par_p)
  end subroutine do_it4

  subroutine do_it3 (par1_t)
    class (test), target :: par1_t
    ! target -> pointer
    ! { dg-final { scan-tree-dump "par_p\[^\n]*VIEW_CONVERT_EXPR\[^\n]*par1_t" "original" } }
    call do_it4 (par1_t)
  end subroutine do_it3

  subroutine do_it2 (par_t)
    class (test), target :: par_t
    ! target -> target
    ! { dg-final { scan-tree-dump-not "par1_t\[^\n]*VIEW_CONVERT_EXPR\[^\n]*par_t" "original" } }
    call do_it3 (par_t)
  end subroutine do_it2

  subroutine do_it1 (par1_a)
    class (test), allocatable :: par1_a
    ! allocatable -> target
    ! { dg-final { scan-tree-dump "par_t\[^\n]*VIEW_CONVERT_EXPR\[^\n]*par1_a" "original" } }
    call do_it2 (par1_a)
  end subroutine do_it1

  subroutine do_it (par_a)
    class (test), allocatable :: par_a
    ! allocatable -> allocatable
    ! { dg-final { scan-tree-dump-not "par1_a\[^\n]*VIEW_CONVERT_EXPR\[^\n]*par_a" "original" } }
    call do_it1 (par_a)
  end subroutine do_it

end module test_module

use test_module

  implicit none
  class (test), allocatable :: var_a
  class (test), pointer :: var_p


  allocate (var_a)
  allocate (var_p)
  var_a%x = 0
  var_p%x = 0
  
  ! allocatable -> allocatable
  ! { dg-final { scan-tree-dump-not "par_a\[^\n]*VIEW_CONVERT_EXPR\[^\n]*var_a" "original" } }
  call do_it (var_a)
  ! allocatable -> target
  ! { dg-final { scan-tree-dump "par_t\[^\n]*VIEW_CONVERT_EXPR\[^\n]*var_a" "original" } }
  call do_it2 (var_a)
  ! pointer -> target
  ! { dg-final { scan-tree-dump "par_t\[^\n]*VIEW_CONVERT_EXPR\[^\n]*var_p" "original" } }
  call do_it2 (var_p)
  ! pointer -> pointer
  ! { dg-final { scan-tree-dump-not "par_p\[^\n]*VIEW_CONVERT_EXPR\[^\n]*var_p" "original" } }
  call do_it4 (var_p)
  if (var_a%x .ne. 2) call abort()
  if (var_p%x .ne. 2) call abort()
  deallocate (var_a)
  deallocate (var_p)
end
