! { dg-do run }
module m
   implicit none
   private
   public tol, z
   integer :: tol = 42
   integer :: z = 42
end module m

subroutine no_only_clause
   use m
   implicit none
   character(len=20) :: str = "&v z=1 tol=1/"
   namelist /v/ tol, z
   read(str, nml=v)
   if (z /= 1) stop 3
   if (tol /= 1) stop 4
end subroutine no_only_clause

subroutine only_clause_without_renaming
   use m, only : tol, z
   implicit none
   character(len=20) :: str = "&v z=1 tol=1/"
   namelist /v/ z, tol
   read(str, nml=v)
   if (z /= 1) stop 3
   if (tol /= 1) stop 4
end subroutine only_clause_without_renaming

subroutine only_clause_with_renaming
   use m, only: y => tol, z
   implicit none
   character(len=20) :: str = "&v z=1 y=1/"
   real :: tol = 0
   namelist /v/ y, z
   read(str, nml=v)
   if (z /= 1) stop 3
   if (y /= 1) stop 4
   if (tol /= 0) stop 5
end subroutine only_clause_with_renaming

program p
   call no_only_clause
   call only_clause_without_renaming
   call only_clause_with_renaming
end program p
