! { dg-do compile }
!
! PR fortran/99369
!
! Contributed by G. Steinmetz
!

module m1bcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabc
   type tabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabc
   end type
   interface operator (.oabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabc.)
      procedure fabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabc
   end interface
contains
   function fabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabc &
        (uabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabc, &
         vabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabc)
      type (tabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabc), intent(in) :: &
         uabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabc, &
         vabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabc
   end
end
subroutine p1
   use m1bcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabc
   type (tabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabc) :: &
      uabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabc, &
      vabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabc
   wabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabc = &
      uabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabc &
     .oabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabc. &
      vabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabc
end


module m2bcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcd
   type tabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcd
   end type
   interface operator (.oabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcd.)
      procedure fabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcd
   end interface
contains
   function fabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcd &
        (uabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcd, &
         vabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcd)
      type (tabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcd), intent(in) :: &
         uabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcd, &
         vabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcd
   end
end
subroutine p2
   use m2bcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcd
   type (tabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcd) :: &
      uabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcd, &
      vabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcd
   wabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcd = &
      uabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcd &
     .oabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcd. &
      vabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcd
end


module m3bcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzab
   type tabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzab
   end type
   interface operator (.oabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzab.)
      procedure fabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzab
   end interface
contains
   function fabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzab &
        (uabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzab, &
         vabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzab)
      type (tabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzab), intent(in) :: &
         uabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzab, &
         vabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzab
   end
end
subroutine p3
   use m3bcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzab
   type (tabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzab) :: &
      uabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzab, &
      vabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzab
   wabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzab = &
      uabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzab &
     .oabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzab. &
      vabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzabcdefgxyzab
end

program main
  call p1
  call p2
  call p3
end
