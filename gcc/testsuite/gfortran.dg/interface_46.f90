! { dg-do compile }
! PR 94090 - this used to cause an ICE.
!  Test case by José Rui Faustino de Sousa.
function cntf(a) result(s)
  implicit none

  integer, intent(in) :: a(:)
  
  integer :: s(3)
  
  s = [1, 2, 3]
  return
end function cntf

program ice_p

  implicit none

  interface
    function cntf(a) result(s)  ! { dg-error "Rank mismatch in function result" }
      implicit none
      integer, intent(in) :: a(:)
      integer             :: s ! (3) <- Ups!
    end function cntf
  end interface

  integer, parameter :: n = 9

  integer :: arr(n)
  
  integer :: s(3)

  s = cntf(arr)
  stop

end program ice_p
