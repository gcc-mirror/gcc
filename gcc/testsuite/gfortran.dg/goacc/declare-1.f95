! { dg-do compile } 

program test
  implicit none
  integer :: i

  !$acc declare copy(i)
contains
  real function foo(n)
    integer, value :: n
    BLOCK
       integer i
       !$acc declare copy(i) ! { dg-error "is not allowed" }
    END BLOCK
  end function foo
end program test
