! { dg-do compile }
! { dg-additional-options "-fcheck=bounds -g -fdump-tree-original" }
!
! PR fortran/30802 - improve bounds-checking for array references
!
! Use proper array component references in runtime error message.

program test
  implicit none
  integer :: k = 0
  type t
     real, dimension(10,20,30) :: z = 23
  end type t
  type u
     type(t) :: vv(4,5)
     complex :: cc(6,7)
  end type u
  type vec
     integer :: xx(3) = [2,4,6]
  end type vec
  type(t) :: uu,     ww(1)
  type(u) :: x1, x2, y1(1), y2(1)

  print *, uu   % z(1,k,:)           ! runtime check for dimension 2 of uu%z
  print *, ww(1)% z(1,:,k)           ! runtime check for dimension 3 of ww...%z
  print *, x1   % vv(2,3)% z(1,:,k)  ! runtime check for dimension 3 of x1...%z
  print *, x2   % vv(k,:)% z(1,2,3)  ! runtime check for dimension 1 of x2%vv
  print *, y1(k)% vv(2,3)% z(k,:,1)  ! runtime check for dimension 1 of y1
                                     !           and for dimension 1 of y1...%z
  print *, y2(1)% vv(:,k)% z(1,2,k)  ! runtime check for dimension 2 of y2...%vv
                                     !           and for dimension 3 of y2...%z
  print *, y1(1)% cc(k,:)% re        ! runtime check for dimension 1 of y1...%cc
contains
  subroutine sub (yy, k)
    class(vec), intent(in) :: yy(:)
    integer,    intent(in) :: k
    print *, yy(1)%xx(k)             ! runtime checks for yy and yy...%xx
  end
end program test

! { dg-final { scan-tree-dump-times "dimension 2 of array .'uu%%z.' outside of expected range" 2 "original" } }
! { dg-final { scan-tree-dump-times "dimension 3 of array .'ww\.\.\.%%z.' outside of expected range" 2 "original" } }
! { dg-final { scan-tree-dump-times "dimension 3 of array .'x1\.\.\.%%z.' outside of expected range" 2 "original" } }
! { dg-final { scan-tree-dump-times "dimension 1 of array .'x2%%vv.' outside of expected range" 2 "original" } }
! { dg-final { scan-tree-dump-times "dimension 1 of array .'y1\.\.\.%%z.' outside of expected range" 2 "original" } }
! { dg-final { scan-tree-dump-times "dimension 2 of array .'y2\.\.\.%%vv.' outside of expected range" 2 "original" } }
! { dg-final { scan-tree-dump-times "dimension 1 of array .'y1\.\.\.%%cc.' outside of expected range" 2 "original" } }

! { dg-final { scan-tree-dump-times "dimension 1 of array .'y1.' above upper bound" 1 "original" } }
! { dg-final { scan-tree-dump-times "dimension 1 of array .'y1.' below lower bound" 1 "original" } }
! { dg-final { scan-tree-dump-times "dimension 3 of array .'y2\.\.\.%%z.' above upper bound" 1 "original" } }
! { dg-final { scan-tree-dump-times "dimension 3 of array .'y2\.\.\.%%z.' below lower bound" 1 "original" } }

! { dg-final { scan-tree-dump-times "dimension 1 of array .'yy.' above upper bound" 1 "original" } }
! { dg-final { scan-tree-dump-times "dimension 1 of array .'yy\.\.\.%%xx.' above upper bound" 1 "original" } }
! { dg-final { scan-tree-dump-times "dimension 1 of array .'yy\.\.\.%%xx.' below lower bound" 1 "original" } }
