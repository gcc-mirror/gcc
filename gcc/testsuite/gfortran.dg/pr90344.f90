! { dg-do compile }
! { dg-additional-options "-ffrontend-optimize" }
! PR 90344 - this used to ICE.
! Test case by Urban Jost.
module M_xterm
contains
   elemental function func1(ch) result(res)
      character,intent(in) :: ch
      logical              :: res
      res=.true.
   end function func1
   elemental function func2(ch) result(res)
      character,intent(in) :: ch
      logical              :: res
      res=.false.
   end function func2
   pure function s2a(string)  RESULT (array)
      character(len=*),intent(in) :: string
      character(len=1)            :: array(len(string))
      forall(i=1:len(string)) array(i) = string(i:i)
   end function s2a
   subroutine sub1()
      write(*,*)all(func1(s2a('ABCDEFG')).or.func2(s2a('ABCDEFG')))
   end subroutine sub1
end module M_xterm
