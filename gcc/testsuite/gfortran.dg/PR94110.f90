! { dg-do compile }
!
! Test the fix for PR94110
! 
  
program asa_p

  implicit none

  integer, parameter :: n = 7

  type t
  end type t

  interface
    subroutine fc2 (x)
      import :: t
      class(t), pointer, intent(in) :: x(..)
    end subroutine
  end interface

  integer :: p(n)
  integer :: s

  p = 1
  s = sumf_as(p)
  if (s/=n) stop 1
  s = sumf_ar(p)
  if (s/=n) stop 2
  stop

contains

  function sumf_as(a) result(s)
    integer, target, intent(in) :: a(*)

    integer :: s

    s = sum_as(a)   ! { dg-error "Actual argument for .a. cannot be an assumed-size array" } 
    s = sum_p_ds(a) ! { dg-error "Actual argument for .a. cannot be an assumed-size array" } 
    s = sum_p_ar(a) ! { dg-error "Actual argument for .a. cannot be an assumed-size array" } 
    return
  end function sumf_as

  function sumf_ar(a) result(s)
    integer, target, intent(in) :: a(..)

    integer :: s

    select rank(a)
    rank(*)
      s = sum_as(a)   ! { dg-error "Actual argument for .a. cannot be an assumed-size array" } 
      s = sum_p_ds(a) ! { dg-error "Actual argument for .a. cannot be an assumed-size array" } 
      s = sum_p_ar(a) ! { dg-error "Actual argument for .a. cannot be an assumed-size array" } 
    rank default
      stop 3
    end select
    return
  end function sumf_ar

  function sum_as(a) result(s)
    integer, intent(in) :: a(:)
  
    integer :: s

    s = sum(a)
    return
  end function sum_as

  function sum_p_ds(a) result(s)
    integer, pointer, intent(in) :: a(:)
  
    integer :: s

    s = -1
    if(associated(a))&
      s = sum(a)
    return
  end function sum_p_ds

  function sum_p_ar(a) result(s)
    integer, pointer, intent(in) :: a(..)
  
    integer :: s

    s = -1
    select rank(a)
    rank(1)
      if(associated(a))&
        s = sum(a)
    rank default
      stop 4
    end select
    return
  end function sum_p_ar

  subroutine sub1(y)
    type(t), target :: y(*)
    call fc2 (y) ! { dg-error "Actual argument for .x. cannot be an assumed-size array" } 
  end subroutine sub1

end program asa_p

