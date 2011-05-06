! { dg-do compile }
!
! PR fortran/48788
!
! Contributed by Zdenek Sojka
!
function foo ()
end function foo
  character(4), external :: foo ! { dg-error "Return type mismatch of function" }
  character(4) :: x
  x = foo ()
END
