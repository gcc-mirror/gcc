! { dg-do compile }
!
! Was ICEing before
!
! Contributed by Reinhold Bader
!

module mod_fin_04
  implicit none
  type :: p_vec
  contains
     final :: delete
  end type p_vec
  type, extends(p_vec) :: bar
  contains
    final :: del2
  end type bar
contains
  subroutine delete(this)
    type(p_vec) :: this
  end subroutine delete
  subroutine del2(this)
    type(bar) :: this
  end subroutine del2
end module
