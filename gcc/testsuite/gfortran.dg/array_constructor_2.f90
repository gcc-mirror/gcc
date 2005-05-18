! { dg-do compile }
! Check that array constructor delimiters match
program bracket_array_constr_2
    implicit none
    integer :: a(4)
    a = (/ 1, 2, 3, 4 ] ! { dg-error "array constructor" }
    a = (/ [ 1, 2, 3, 4 /) ] ! { dg-error "array constructor" }
end program bracket_array_constr_2
