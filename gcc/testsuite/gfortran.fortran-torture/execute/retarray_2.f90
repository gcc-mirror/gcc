! Procedure to test module procedures returning arrays.
! The array spec only gets applied to the result variable, not the function
! itself.  As a result we missed it during resolution, and used the wrong
! calling convention (functions returning arrays must always have explicit
! interfaces).
module retarray_2
contains
  function z(a) result (aout)
  integer, dimension(4) :: aout,a
  aout = a
  end function z
end module retarray_2

program retarray
  use retarray_2
  integer, dimension(4) :: b, a=(/1,2,3,4/)
  b =  z(a)
  if (any (b .ne. (/1, 2, 3, 4/))) call abort
end

