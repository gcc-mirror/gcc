! This uncovered a bug in the reading/writing of expressions.
module module_expr_1
   integer a
end module

module module_expr_2
   use module_expr_1
contains

subroutine myproc (p)
   integer, dimension (a) :: p
end subroutine
end module

program module_expr
   use module_expr_1
   use module_expr_2
end program
