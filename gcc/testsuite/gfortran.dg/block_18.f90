! { dg-do compile }
!
! PR fortran/126127 - resolution of user-defined operators in named BLOCK

module geom2d
  use, intrinsic :: iso_fortran_env, only: wp => real64
  implicit none
  private
  public :: vec2_t, operator(.dot.)

  type :: vec2_t
     real(wp) :: x = 0, y = 0
  end type vec2_t

  interface operator(.dot.)
     module procedure vec_dot
  end interface operator(.dot.)
contains
  pure function vec_dot(a, b) result(r)
    type(vec2_t), intent(in) :: a, b
    real(wp)                 :: r
    r = a%x*b%x + a%y*b%y
  end function vec_dot
end module

module consumer
  use, intrinsic :: iso_fortran_env, only: r8 => real64
  use geom2d, only: vec2_t, operator(.dot.)
  implicit none
contains
  subroutine f(id)
    integer, intent(out) :: id
    type(vec2_t) :: d
    real(r8)     :: dd
    id = 0
    blk: block
      dd = d .dot. d
    end block blk
  end subroutine f

  subroutine f2(id)
    integer, intent(out) :: id
    type(vec2_t) :: d
    real(r8)     :: dd
    id = 0
    block
      dd = d .dot. d
    end block
  end subroutine f2

  subroutine f3(id)
    integer, intent(out) :: id
    type(vec2_t) :: d
    real(r8)     :: dd
    block
      blk1: block
        id = 0
        block
          blk2: block
            dd = d .dot. d
          end block blk2
        end block
      end block blk1
    end block
  end subroutine f3
end module
