! { dg-do compile }
!
! A function returning a pointer cannot be interoperable
! and cannot be used as FPTR argument to C_F_POINTER.

subroutine s ()
  use, intrinsic :: iso_c_binding
  implicit none
  type(c_ptr) :: cPtr
  call c_f_pointer (cPtr, p0)            ! { dg-error "function returning a pointer" }
  call c_f_pointer (cPtr, p1, shape=[2]) ! { dg-error "function returning a pointer" }
contains
  function p0 ()
    integer, pointer :: p0
    nullify (p0)
  end
  function p1 ()
    integer, pointer :: p1(:)
    nullify (p1)
  end
  function fp0 ()
    integer, pointer :: fp0
    call c_f_pointer (cPtr, fp0)            ! valid here
  end
  function fp1 ()
    integer, pointer :: fp1(:)
    call c_f_pointer (cPtr, fp1, shape=[2]) ! valid here
  end
  function ffp0 () result (fp0)
    integer, pointer :: fp0
    call c_f_pointer (cPtr, fp0)            ! valid here
  end
  function ffp1 () result (fp1)
    integer, pointer :: fp1(:)
    call c_f_pointer (cPtr, fp1, shape=[2]) ! valid here
  end
end
