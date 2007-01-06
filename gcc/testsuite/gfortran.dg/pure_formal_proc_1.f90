! { dg-do compile }
! Test fix for PR30034 in which the legal, pure procedure formal
! argument was rejected as an error.
!
! Contgributed by Troban Trumsko <trumsko@yahoo.com>
!
 pure subroutine s_one ( anum, afun )
    integer, intent(in) :: anum
    interface
      pure function afun (k) result (l)
        implicit none
        integer, intent(in) :: k
        integer :: l
      end function afun
    end interface
end subroutine s_one
