! { dg-do compile }
!
! PR fortran/114475
! The array specification of PP in OL_EVAL used to be rejected in the submodule
! because the compiler was not able to see the host-association of N_EXTERNAL
! there.
!
! Contributed by Jürgen Reuter <juergen.reuter@desy.de>.

module t1
  use, intrinsic :: iso_c_binding
  implicit none
  private
  public :: t1_t
  integer :: N_EXTERNAL = 0

  type :: t1_t
  contains
    procedure :: set_n_external => t1_set_n_external
  end type t1_t

  abstract interface
     subroutine ol_eval (id, pp, emitter) bind(C)
       import
       real(kind = c_double), intent(in) :: pp(5 * N_EXTERNAL)
     end subroutine ol_eval
  end interface
  interface
    module subroutine t1_set_n_external (object, n)
      class(t1_t), intent(inout) :: object
      integer, intent(in) :: n
    end subroutine t1_set_n_external
  end interface

end module t1

submodule (t1) t1_s
  implicit none
contains
  module subroutine t1_set_n_external (object, n)
    class(t1_t), intent(inout) :: object
    integer, intent(in) :: n
    N_EXTERNAL = n
  end subroutine t1_set_n_external

end submodule t1_s
