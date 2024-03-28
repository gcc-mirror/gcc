! { dg-do compile }
! { dg-additional-options "-std=legacy" }
!
! PR fortran/50410
!
! Silently allow overlapping initialization in legacy mode (used to ICE)

program p
  implicit none
  type t
     integer :: g = 1
  end type t
  type(t) :: u = t(2)
  data u%g /3/
  print *, u    ! this might print "2"
end
