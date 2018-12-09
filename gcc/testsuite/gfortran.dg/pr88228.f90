! { dg-do compile }
! { dg-options "-fdec" }
! PR fortran/88228
program p
   integer :: n = .not. 1
   integer :: j = .true. .or. 1
end

