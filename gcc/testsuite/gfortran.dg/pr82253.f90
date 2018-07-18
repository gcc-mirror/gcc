! PR middle-end/82253
! { dg-do compile { target fortran_real_16 } }
! { dg-options "-Og" }

module pr82253
  implicit none
  private
  public :: static_type
  type, public :: T
    procedure(), nopass, pointer :: testProc => null()
  end type
  type, public :: S
    complex(kind=16), pointer :: ptr
  end type
  type(T), target :: type_complex32
  interface static_type
    module procedure foo
  end interface
  interface
    subroutine bar (testProc)
      procedure(), optional :: testProc
    end subroutine
  end interface
  contains
    function foo (self) result(res)
      complex(kind=16) :: self
      type(T), pointer :: res
      call bar (testProc = baz)
    end function
    subroutine baz (buffer, status)
      character(len=*) :: buffer
      integer(kind=4) :: status
      complex(kind=16), target :: obj
      type(S) :: self
      integer(kind=1), parameter :: zero(storage_size(obj)/8) = 0
      obj = transfer (zero, obj)
      self%ptr => obj
      write (buffer, *, iostat=status) self%ptr, '#'
    end subroutine
end module pr82253
