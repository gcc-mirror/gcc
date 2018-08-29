! PR fortran/85701
! { dg-do compile }

subroutine s1
   !$acc declare copy(s1) ! { dg-error "is not a variable" }
end

subroutine s2
   !$acc declare present(s2) ! { dg-error "is not a variable" }
end

function f1 ()
   !$acc declare copy(f1) ! { dg-error "is not a variable" }
end

function f2 ()
   !$acc declare present(f2) ! { dg-error "is not a variable" }
end

program p
  !$acc declare copy(p) ! { dg-error "is not a variable" }
end

