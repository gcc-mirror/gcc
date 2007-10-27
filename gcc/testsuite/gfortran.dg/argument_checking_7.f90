! { dg-do compile }
! PR31306 ICE with implicit character variables
! Test case from PR and prepared by Jerry DeLisle <jvdelisle@gcc.gnu.org>
module cyclic
  implicit none
  contains
    function ouch(x,y) ! { dg-error "has no IMPLICIT type" }
      implicit character(len(ouch)) (x) ! { dg-error "Conflict in attributes" }
      implicit character(len(x)+1) (y)
      implicit character(len(y)-1) (o)
      intent(in) x,y
      character(len(y)-1) ouch
      integer i
      do i = 1, len(ouch)
        ouch(i:i) = achar(ieor(iachar(x(i:i)),iachar(y(i:i)))) ! { dg-error " PROCEDURE attribute conflicts" }
      end do
      end function ouch
end module cyclic
