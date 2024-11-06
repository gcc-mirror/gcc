! { dg-do run }
! { dg-options "-funsigned" }
! Check compile-time simplification of FINDLOC
! Mostly lifted from findloc_5.f90.
program memain
  implicit none
  call test1
  call test2
contains
  subroutine test1
    unsigned, dimension(4) :: a1
    integer :: i1, i2, i3, i4
    unsigned, dimension(2,2) :: a, b
    integer, dimension(2) :: t8, t9, t10
    unsigned, dimension(2,3) :: c
    integer, dimension(3) :: t13
    integer, dimension(2) :: t14

    a1 = [1u,  2u,  3u,  1u]
    i1 = findloc(a1, 1u, dim=1)
    if (i1 /= 1) stop 1
    i2 = findloc(a1, 2u, dim=1)
    if (i2 /= 2) stop 2
    i3 = findloc(a1,3u, dim=1)
    if (i3 /= 3) stop 3
    i4 = findloc(a1, 1u, dim=1, back=.true.)
    if (i4 /= 4) stop 4
    a = reshape([1u,2u,3u,4u], [2,2])
    b = reshape([1u,2u,1u,2u], [2,2])
    t8 = findloc(a,5u)
    if (any(t8 /= [0,0])) stop 8
    t9 = findloc(a,5u,back=.true.)
    if (any(t9 /= [0,0])) stop 9
    c = reshape([1u,2u,2u,2u,-9u,6u], [2,3])
    t13 = findloc (c, value=2u, dim=1)
    if (any(t13 /= [2,1,0])) stop 13
    t14 = findloc (c, value=2u, dim=2)
    if (any(t14 /= [2,1])) stop 14
  end subroutine test1
  subroutine test2
    unsigned,  dimension(4),  parameter :: a1 = [1u,  2u,  3u,  1u]
    integer,  parameter :: i1 = findloc(a1, 1u, dim=1)
    integer,  parameter :: i2 = findloc(a1, 2u, dim=1)
    integer,  parameter :: i3 = findloc(a1, 3u, dim=1)
    integer,  parameter :: i4 = findloc(a1, 1u, dim=1, back=.true.)
    integer,  parameter :: i0 = findloc(a1, -1u, dim=1)
    logical,  dimension(4),  parameter :: msk = [.false., .true., .true., .true.]
    integer,  parameter :: i4a = findloc(a1, 1u, dim=1, mask=msk)
    integer,  parameter :: i4b = findloc(a1, 1u, dim=1, mask=msk, back=.true.)
    unsigned, dimension(2,2), parameter :: a = reshape([1u,2u,3u,4u], [2,2]), &
       b =  reshape([1u,2u,1u,2u], [2,2])
    integer, parameter, dimension(2) :: t8 = findloc(a, 5u), t9 = findloc(a, 5u, back=.true.)
    integer, parameter, dimension(2) :: t10= findloc(a, 2u), t11= findloc(a, 2u, back=.true.)
    logical, dimension(2,2), parameter :: lo = reshape([.true., .false., .true., .true. ], [2,2])
    integer, parameter, dimension(2) :: t12 = findloc(b,2u, mask=lo)

    unsigned, dimension(2,3), parameter :: c = reshape([1u,2u,2u,2u,-9u,6u], [2,3])
    integer, parameter, dimension(3) :: t13 = findloc(c, value=2u, dim=1)
    integer, parameter, dimension(2) :: t14 = findloc(c, value=2u, dim=2)

    if (i1 /= 1) stop 1
    if (i2 /= 2) stop 2
    if (i3 /= 3) stop 3
    if (i4 /= 4) stop 4
    if (i0 /= 0) stop 5
    if (i4a /= 4) stop 6
    if (i4b /= 4) stop 7
    if (any(t8 /= [0,0])) stop 8
    if (any(t9 /= [0,0])) stop 9
    if (any(t10 /= [2,1])) stop 10
    if (any(t11 /= [2,1])) stop 11
    if (any(t12 /= [2,2])) stop 12
    if (any(t13 /= [2,1,0])) stop 13
    if (any(t14 /= [2,1])) stop 14
  end subroutine test2
end program memain
