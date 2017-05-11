! { dg-do compile }
! PR78659 Spurious "requires DTIO" reported against namelist statement
program p
   type t
     integer :: k
   end type
   class(t), allocatable :: x
   namelist /nml/ x
end
