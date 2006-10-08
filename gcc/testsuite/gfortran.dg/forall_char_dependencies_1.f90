! { dg-do compile }
! Tests fix for PR29211, in which an ICE would be produced by FORALL assignments
! with dependencies.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
  character(12), dimension(2) :: a, b
  a= (/"abcdefghijkl","mnopqrstuvwx"/)
! OK because it uses gfc_trans_assignment
  forall (i=1:2) b(i) = a(i)
! Was broken - gfc_trans_assign_need_temp had no handling of string lengths
  forall (i=1:2) a(3-i) = a(i)
end
