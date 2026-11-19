! { dg-do run }
!
! Contributed by Jerry Delisle  <delisle@gcc.gnu.org>
!
module pdt_kind_init_m
  implicit none
! real(k) :: x = 0 in a PDT with KIND parameter k was rejected:
! "Cannot convert INTEGER(4) to REAL(0)".  Valid (F2018 7.5.4.4)
! 16.2.1 rejects it too.
  type t(k)
    integer, kind :: k = kind(1.0)
    real(k) :: x = 0
  end type
end module


program p
  call pst_kind_param_real_init
contains
  subroutine pst_kind_param_real_init
    use pdt_kind_init_m
    implicit none
    type(t) :: a
    type(t(kind(1.0d0))) :: b
    if (a%x /= 0 .or. b%x /= 0) stop 1
  end subroutine pst_kind_param_real_init
end program
