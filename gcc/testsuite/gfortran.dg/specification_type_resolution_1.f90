! { dg-do compile }
! Test of the fix of PR27089, where gfortran was unable to resolve the
! type of n_elements_uncommon_with_ in the specification expression on
! line 21.
!
! Test extracted from vec{int}.F90 of tonto.
!
module test
   public    n_elements_uncommon_with_
   interface n_elements_uncommon_with_
      module procedure n_elements_uncommon_with
   end interface
contains
   pure function n_elements_uncommon_with(x) result(res)
      integer(4), dimension(:), intent(in) :: x
      integer(4) :: res
      res = size (x, 1)
   end function
   pure function elements_uncommon_with(x) result(res)
      integer(4), dimension(:), intent(in) :: x
      integer(4), dimension(n_elements_uncommon_with_(x)) :: res
      res = x
   end function
end module test
   use test
   integer(4) :: z(4)
   z = 1
   print *, elements_uncommon_with (z)
   print *, n_elements_uncommon_with_ (z)
end
