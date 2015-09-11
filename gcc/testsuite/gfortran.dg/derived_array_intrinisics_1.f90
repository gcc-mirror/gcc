! { dg-do compile }
! { dg-options "-fdump-tree-original" }
! Test the fix for PR45081 in which derived type array valued intrinsics failed
! to simplify, which caused an ICE in trans-array.c
!
! Contributed by Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
! 
  module m
    implicit none
    integer :: i
    type t
      integer :: i
    end type t
    type(t), dimension(4), parameter :: t1  = [( t(i), i = 1, 4)]
    type(t), dimension(4), parameter :: t2  = [( t(i), i = 8, 11)]
    type(t), dimension(2,2), parameter :: a = reshape ( t1, [ 2, 2 ] )
    type(t), dimension(2,2), parameter :: b = transpose (a)
    type(t), dimension(4), parameter :: c = reshape ( b, [ 4 ] )
    type(t), dimension(2), parameter :: d = pack ( c, [.false.,.true.,.false.,.true.])
    type(t), dimension(4), parameter :: e = unpack (d, [.false.,.true.,.false.,.true.], t2)
    type(t), dimension(4,2), parameter :: f = spread (e, 2, 2)
    type(t), dimension(8), parameter :: g = reshape ( f, [ 8 ] )
    integer, parameter :: total = sum(g%i)
  end module m

    use m
    integer :: j
    j = total
  end
! { dg-final { scan-tree-dump-times "j = 50" 1 "original" } }
