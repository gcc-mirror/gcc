! { dg-do run }
! { dg-additional-sources PR100915.c }
!
! Test the fix for PR100915
! 

module isof_m

  use, intrinsic :: iso_c_binding, only: &
    c_signed_char, c_int16_t
  
  implicit none

  private
  
  public :: &
    CFI_type_cptr, CFI_type_cfunptr
  
  public ::      &
    check_fn_as, &
    check_fn_ar
  
  public :: &
    mult2
  
  public ::          &
    cfi_encode_type
  
  integer, parameter :: CFI_type_t = c_int16_t
  
  integer(kind=c_int16_t), parameter :: CFI_type_mask = int(z"FF", kind=c_int16_t)
  integer(kind=c_int16_t), parameter :: CFI_type_kind_shift = 8_c_int16_t

  ! Intrinsic types. Their kind number defines their storage size. */
  integer(kind=c_signed_char), parameter :: CFI_type_cptr   = 7
  integer(kind=c_signed_char), parameter :: CFI_type_cfunptr   = 8

  interface
    subroutine check_fn_as(a, t, k, e, n) &
      bind(c, name="check_fn")
      use, intrinsic :: iso_c_binding, only: &
        c_int16_t, c_signed_char, c_size_t
      implicit none
      type(*),                       intent(in) :: a(:)
      integer(c_int16_t),     value, intent(in) :: t
      integer(c_signed_char), value, intent(in) :: k
      integer(c_size_t),      value, intent(in) :: e
      integer(c_size_t),      value, intent(in) :: n
    end subroutine check_fn_as
    subroutine check_fn_ar(a, t, k, e, n) &
      bind(c, name="check_fn")
      use, intrinsic :: iso_c_binding, only: &
        c_int16_t, c_signed_char, c_size_t
      implicit none
      type(*),                       intent(in) :: a(..)
      integer(c_int16_t),     value, intent(in) :: t
      integer(c_signed_char), value, intent(in) :: k
      integer(c_size_t),      value, intent(in) :: e
      integer(c_size_t),      value, intent(in) :: n
    end subroutine check_fn_ar
  end interface

contains

  function mult2(a) result(b) bind(c)
    use, intrinsic :: iso_c_binding, only: &
      c_int
  
    integer(kind=c_int), value, intent(in) :: a

    integer(kind=c_int) :: b

    b = 2_c_int * a
    return
  end function mult2
  
  elemental function cfi_encode_type(type, kind) result(itype)
    integer(kind=c_signed_char), intent(in) :: type
    integer(kind=c_signed_char), intent(in) :: kind

    integer(kind=c_int16_t) :: itype, ikind

    itype = int(type, kind=c_int16_t)
    itype = iand(itype, CFI_type_mask)
    ikind = int(kind, kind=c_int16_t)
    ikind = iand(ikind, CFI_type_mask)
    ikind = shiftl(ikind, CFI_type_kind_shift)
    itype = ior(ikind, itype)
    return
  end function cfi_encode_type
  
end module isof_m

module iso_check_m

  use, intrinsic :: iso_c_binding, only: &
    c_signed_char, c_int16_t, c_size_t

  use, intrinsic :: iso_c_binding, only: &
    c_funptr, c_funloc, c_associated

  use :: isof_m, only:  &
    CFI_type_cptr, CFI_type_cfunptr
  
  use :: isof_m, only: &
    check_fn_as,       &
    check_fn_ar
  
  use :: isof_m, only: &
    mult2
  
  use :: isof_m, only: &
    cfi_encode_type
  
  implicit none

  integer                           :: i
  integer(kind=c_size_t), parameter :: b = 8
  integer,                parameter :: n = 11
  
contains

  subroutine check_c_funptr()
    type(c_funptr) :: p(n)
    integer :: i
    !
    p = [(c_funloc(mult2), i=1,n)]
    call f_check_c_funptr_as(p)
    do i = 1, n
      if(.not.c_associated(p(i), c_funloc(mult2))) stop 1
    end do
    p = [(c_funloc(mult2), i=1,n)]
    call c_check_c_funptr_as(p)
    do i = 1, n
      if(.not.c_associated(p(i), c_funloc(mult2))) stop 2
    end do
    p = [(c_funloc(mult2), i=1,n)]
    call f_check_c_funptr_ar(p)
    do i = 1, n
      if(.not.c_associated(p(i), c_funloc(mult2))) stop 3
    end do
    p = [(c_funloc(mult2), i=1,n)]
    call c_check_c_funptr_ar(p)
    do i = 1, n
      if(.not.c_associated(p(i), c_funloc(mult2))) stop 4
    end do
    return
  end subroutine check_c_funptr

  subroutine f_check_c_funptr_as(a)
    type(c_funptr), intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e
    !
    k = 0
    e = storage_size(a)/b
    t = cfi_encode_type(CFI_type_cfunptr, k)
    ! Assumes 64-bit target.
    ! if(e/=8) stop 5
    do i = 1, n
      if(.not.c_associated(a(i), c_funloc(mult2))) stop 6
    end do
    call check_fn_as(a, t, k, e, 1_c_size_t)
    do i = 1, n
      if(.not.c_associated(a(i), c_funloc(mult2))) stop 7
    end do
    return
  end subroutine f_check_c_funptr_as

  subroutine c_check_c_funptr_as(a) bind(c)
    type(c_funptr), intent(in) :: a(:)
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e
    !
    k = 0
    e = storage_size(a)/b
    t = cfi_encode_type(CFI_type_cfunptr, k)
    ! Assumes 64-bit target.
    ! if(e/=8) stop 8
    do i = 1, n
      if(.not.c_associated(a(i), c_funloc(mult2))) stop 9
    end do
    call check_fn_as(a, t, k, e, 1_c_size_t)
    do i = 1, n
      if(.not.c_associated(a(i), c_funloc(mult2))) stop 10
    end do
    return
  end subroutine c_check_c_funptr_as

  subroutine f_check_c_funptr_ar(a)
    type(c_funptr), intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e
    !
    k = 0
    e = storage_size(a)/b
    t = cfi_encode_type(CFI_type_cfunptr, k)
    ! Assumes 64-bit target.
    ! if(e/=8) stop 11
    select rank(a)
    rank(1)
      do i = 1, n
        if(.not.c_associated(a(i), c_funloc(mult2))) stop 12
      end do
    rank default
      stop 13
    end select
    call check_fn_ar(a, t, k, e, 1_c_size_t)
    select rank(a)
    rank(1)
      do i = 1, n
        if(.not.c_associated(a(i), c_funloc(mult2))) stop 14
      end do
    rank default
      stop 15
    end select
    return
  end subroutine f_check_c_funptr_ar

  subroutine c_check_c_funptr_ar(a) bind(c)
    type(c_funptr), intent(in) :: a(..)
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e
    !
    k = 0
    e = storage_size(a)/b
    t = cfi_encode_type(CFI_type_cfunptr, k)
    ! Assumes 64-bit target.
    ! if(e/=8) stop 16
    select rank(a)
    rank(1)
      do i = 1, n
        if(.not.c_associated(a(i), c_funloc(mult2))) stop 17
      end do
    rank default
      stop 18
    end select
    call check_fn_ar(a, t, k, e, 1_c_size_t)
    select rank(a)
    rank(1)
      do i = 1, n
        if(.not.c_associated(a(i), c_funloc(mult2))) stop 19
      end do
    rank default
      stop 20
    end select
    return
  end subroutine c_check_c_funptr_ar

end module iso_check_m

program main_p
  
  use :: iso_check_m, only: &
    check_c_funptr

  implicit none

  call check_c_funptr()
  stop

end program main_p

!! Local Variables:
!! mode: f90
!! End:

