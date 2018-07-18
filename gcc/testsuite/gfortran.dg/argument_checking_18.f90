! { dg-do compile }
!
! PR 47349: missing warning: Actual argument contains too few elements
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

 implicit none
 type t
    integer :: j(3)
 end type t

 type(t) :: tt
 integer :: i(3) = (/ 1,2,3 /)

 tt%j = i

 call sub1 (i)     ! { dg-error "Actual argument contains too few elements" }
 call sub1 (tt%j)  ! { dg-error "Actual argument contains too few elements" }
 call sub2 (i)     ! { dg-error "Rank mismatch in argument" }
 call sub2 (tt%j)  ! { dg-error "Rank mismatch in argument" }

contains

  subroutine sub1(i)
    integer, dimension(1:3,1:3) :: i
    print *,"sub1:",i
  end subroutine

  subroutine sub2(i)
    integer, dimension(:,:) :: i
    print *,"sub2:",i
  end subroutine

end
