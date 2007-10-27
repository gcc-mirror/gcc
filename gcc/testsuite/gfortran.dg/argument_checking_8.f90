! { dg-do run }
! PR31306 ICE with implicit character variables
! Test case from PR and prepared by Jerry DeLisle <jvdelisle@gcc.gnu.org>
module cyclic
 implicit none
 contains
   character(10) function ouch(x,y)
     implicit character(len(ouch)) (x)
     implicit character(len(x)+1) (y)
     intent(in) x,y
     integer i
     do i = 1, len(ouch)
        ouch(i:i) = achar(ieor(iachar(x(i:i)),iachar(y(i:i))))
     end do
   end function ouch
end module cyclic

program test
  use cyclic
  implicit none
  character(10) astr
  integer i
  write(astr,'(a)') ouch('YOW!      ','jerry      ')
  if (astr(1:5) /= "3*%SY") call abort
  do i=6,10
    if (astr(i:i) /= achar(0)) call abort
  end do    
end program test
! { dg-final { cleanup-modules "cyclic" } }
