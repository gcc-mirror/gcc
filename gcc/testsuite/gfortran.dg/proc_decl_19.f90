! { dg-do compile }
! 
! PR 36426
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

abstract interface
  function foo(x)
    character(len=*) :: x
    character(len=len(x)) :: foo
  end function foo
end interface
procedure(foo) :: bar

abstract interface
  character function abs_fun()
  end function
end interface
procedure(abs_fun):: x

character(len=20) :: str
str = bar("Hello")
end
