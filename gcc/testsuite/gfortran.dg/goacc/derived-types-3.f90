module bar
  type :: type1
     integer :: a(5)
     integer :: b(5)
  end type
end module

subroutine foo
   use bar
   type(type1) :: var
   !$acc enter data copyin(var%a) copyin(var%a) ! { dg-error ".var\.a. appears more than once in map clauses" }
end subroutine
