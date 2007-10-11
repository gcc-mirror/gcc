! PR fortran/33500

subroutine whatever()
logical(kind=1) :: l1, l2, l3
if ((l1 .and. l2) .neqv. l3) then
   l1 = .true.
endif
end
