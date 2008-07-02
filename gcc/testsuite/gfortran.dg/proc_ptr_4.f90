! { dg-do compile }
!
! PROCEDURE POINTERS & pointer-valued functions
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

interface
  integer function f1()
  end function
end interface

interface
 function f2()
   integer, pointer :: f2
 end function
end interface

interface
 function pp1()
   integer :: pp1
 end function
end interface
pointer :: pp1

pointer :: pp2
interface
  function pp2()
    integer :: pp2
  end function
end interface

pointer :: pp3
interface
  function pp3()
    integer, pointer :: pp3
  end function
end interface

interface
  function pp4()
    integer, pointer :: pp4
  end function
end interface
pointer :: pp4


pp1 => f1

pp2 => pp1

f2 => f1	! { dg-error "is not a variable" }

pp3 => f2

pp4 => pp3

end