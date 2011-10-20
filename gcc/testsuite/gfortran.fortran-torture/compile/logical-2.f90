! Check for operand type validity after gimplification

subroutine whatever()
logical(kind=1) :: l1
logical(kind=2) :: l2
logical(kind=4) :: l3
if ((l1 .and. l2) .neqv. l3) then
   l1 = .true.
endif
end
