! { dg-do compile }

! This test used to ICE during gimplification (PR103898).

Module g
contains
  function mysize(array, dim)
    integer :: mysize
    integer, dimension(:), intent(in)   :: array
    integer, optional,     intent(in)   :: dim
    if (present(dim)) then
       mysize = size(array, dim=dim)
    endif
  end function mysize
end module
