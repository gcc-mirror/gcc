! { dg-do compile }
!
! Fix a regession caused by the first patch for PR84674.
!
! Contributed by Juergen Reuter  <juergen.reuter@desy.de>
!
module m1
  implicit none
  private
  public :: t1
  type, abstract :: t1
  end type t1
end module m1

module t_base
  use m1, only: t1
  implicit none
  private
  public :: t_t
  type, abstract :: t_t
   contains
     procedure (t_out), deferred :: output
  end type t_t

  abstract interface
     subroutine t_out (t, handle)
       import
       class(t_t), intent(inout) :: t
       class(t1), intent(inout), optional :: handle
     end subroutine t_out
  end interface

end module t_base


module t_ascii
  use m1, only: t1
  use t_base
  implicit none
  private

  type, abstract, extends (t_t) :: t1_t
   contains
     procedure :: output => t_ascii_output
  end type t1_t
  type, extends (t1_t) :: t2_t
  end type t2_t
  type, extends (t1_t) :: t3_t
     logical :: verbose = .true.
  end type t3_t

  interface
    module subroutine t_ascii_output &
         (t, handle)
      class(t1_t), intent(inout) :: t
      class(t1), intent(inout), optional :: handle
    end subroutine t_ascii_output
  end interface
end module t_ascii

submodule (t_ascii) t_ascii_s
  implicit none
contains
  module subroutine t_ascii_output &
       (t, handle)
    class(t1_t), intent(inout) :: t
    class(t1), intent(inout), optional :: handle
    select type (t)
    type is (t3_t)
    type is (t2_t)
    class default
       return
    end select
  end subroutine t_ascii_output
end submodule t_ascii_s

