! { dg-do run { target c99_runtime } }
! { dg-additional-sources ISO_Fortran_binding_15.c }
!
! Test the fix for PR921233. The additional source is the main program.
!
! Contributed by Vipul Parekh  <parekhvs@gmail.com>
!
module m
  use, intrinsic :: iso_c_binding, only : c_int
contains
  subroutine Fsub( dat ) bind(C, name="Fsub")
    integer(c_int), allocatable, intent(out) :: dat(..)
    select rank (dat)
      rank (0)
      allocate( dat )
      dat = 42
    end select
    return
  end subroutine
end module m
