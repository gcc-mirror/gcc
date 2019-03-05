! { dg-do compile }
! PR fortran/24549 (and duplicate PR fortran/27487)
module gfcbug29_import
  interface
     subroutine foo (x)
       something :: dp ! { dg-error "Unclassifiable statement" }
       real (kind=dp) :: x ! { dg-error "has not been declared or is a variable, which does not reduce to a constant expression" }
     end subroutine foo
  end interface
end module gfcbug29_import

subroutine FOO
    X :: I
    equivalence (I,I)
end
