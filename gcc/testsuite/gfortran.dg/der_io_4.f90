! { dg-do compile }
! PR41859 ICE on invalid expression involving DT with pointer components in I/O.
! The parens around p below are significant.
  TYPE :: ptype
    character, pointer, dimension(:) :: x => null()
  END TYPE
  TYPE(ptype) :: p
  print *, ((((p))))       ! { dg-error "Data transfer element" }
end
