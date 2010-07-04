   logical :: l1(4)
   logical :: l2(4)
   l1 = (/.TRUE.,.FALSE.,.TRUE.,.FALSE./)
   l2 = (/.FALSE.,.TRUE.,.FALSE.,.TRUE./)
   if (dot_product (l1, l2)) call abort ()
   l2 = .TRUE.
   if (.not.dot_product (l1, l2)) call abort ()
end

