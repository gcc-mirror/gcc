! { dg-do compile }
! { dg-options "-std=f2008" }
!
interface
  subroutine foo()
  end 
  integer function bar()
  end 
end interface
contains
  subroutine test()
  end
  integer function f()
    f = 42
  end
end
