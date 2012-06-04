! { dg-do compile }
! PR fortran/48831
! Contributed by Tobias Burnus

program p1
    implicit none
    integer, parameter  :: i1    = kind(0)
    integer, parameter  :: i2(1) = [i1]
    integer(kind=i2(1)) :: i3

    i3 = int(0, i1)
    print *, i3

    i3 = int(0, i2(1))  ! This line gives an error when compiling.
    print *, i3
end program p1
