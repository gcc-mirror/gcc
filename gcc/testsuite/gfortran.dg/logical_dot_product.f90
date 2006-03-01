! { dg-do run }
! Checks the LOGICAL version of dot_product
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
   logical :: l1(4) = (/.TRUE.,.FALSE.,.TRUE.,.FALSE./)
   logical :: l2(4) = (/.FALSE.,.TRUE.,.FALSE.,.TRUE./)
   if (dot_product (l1, l2)) call abort ()
   l2 = .TRUE.
   if (.not.dot_product (l1, l2)) call abort ()
end