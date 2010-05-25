! { dg-do "compile" }
! { dg-options "-fwhole-file" }
!
! PR fortran/30668
!

integer(8) function two()
  two = 2
end function two

CHARACTER(len=8) function string()
  string = "gfortran"
end function string


program xx
  INTEGER :: a
  CHARACTER(len=4) :: s, string   ! { dg-error "Character length mismatch" }

  a = two()                       ! { dg-error "Return type mismatch" }
  s = string()
end program xx
