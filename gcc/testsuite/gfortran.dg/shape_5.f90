! { dg-do run }
! PR 40067 - this used to segfault on an unallocated return array.
    integer, dimension(10)             :: int1d
    integer, dimension(:), pointer     :: int1d_retrieved

    allocate(int1d_retrieved(10))
    if (any(shape(int1d_retrieved) /= shape(INT1D))) call abort()
    end
