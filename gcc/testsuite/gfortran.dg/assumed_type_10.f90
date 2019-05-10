! { dg-do compile }
! { dg-options "-O0 -fdump-tree-original" }
! PR 61968 - this used to generate invalid assembler containing
! TYPE(*).

module testmod
  use iso_c_binding, only: c_size_t, c_int32_t, c_int64_t
  implicit none

  interface test
    procedure :: test_32
    procedure :: test_array
  end interface test

  interface
    subroutine test_lib (a, len) bind(C, name="xxx")
      use iso_c_binding, only: c_size_t
      type(*), dimension(*) :: a
      integer(c_size_t), value :: len
   end subroutine
  end interface

contains

  subroutine test_32 (a, len)
    type(*), dimension(*) :: a
    integer(c_int32_t), value :: len
    call test_lib (a, int (len, kind=c_size_t))
  end subroutine

  subroutine test_array (a)
    use iso_c_binding, only: c_size_t
    class(*), dimension(..), target :: a
    call test_lib (a, int (sizeof (a), kind=c_size_t))
  end subroutine

end module

  subroutine test_32_ (a, len)
    use iso_c_binding, only: c_int32_t
    use testmod
    type(*), dimension(*) :: a
    integer(c_int32_t), value :: len
    call test (a, len)
  end subroutine
! { dg-final { scan-tree-dump-not "! __vtype_TYPE\\(*\\)" "original" } }
