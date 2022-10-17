! { dg-do run }
! { dg-additional-sources PR100911.c }
!
! Test the fix for PR100911
! 

module isof_m

  use, intrinsic :: iso_c_binding, only: &
    c_signed_char, c_int16_t
  
  implicit none

  private
  
  public :: &
    CFI_type_cptr
 
  public ::      &
    check_tk_as, &
    check_tk_ar
  
  public ::          &
    cfi_encode_type
  
  integer, parameter :: CFI_type_t = c_int16_t
  
  integer(kind=c_int16_t), parameter :: CFI_type_mask = int(z"FF", kind=c_int16_t)
  integer(kind=c_int16_t), parameter :: CFI_type_kind_shift = 8_c_int16_t

  ! Intrinsic types. Their kind number defines their storage size. */
  integer(kind=c_signed_char), parameter :: CFI_type_cptr   = 7

  interface
    subroutine check_tk_as(a, t, k, e, n) &
      bind(c, name="check_tk")
      use, intrinsic :: iso_c_binding, only: &
        c_int16_t, c_signed_char, c_size_t
      implicit none
      type(*),                       intent(in) :: a(:)
      integer(c_int16_t),     value, intent(in) :: t
      integer(c_signed_char), value, intent(in) :: k
      integer(c_size_t),      value, intent(in) :: e
      integer(c_size_t),      value, intent(in) :: n
    end subroutine check_tk_as
    subroutine check_tk_ar(a, t, k, e, n) &
      bind(c, name="check_tk")
      use, intrinsic :: iso_c_binding, only: &
        c_int16_t, c_signed_char, c_size_t
      implicit none
      type(*),                       intent(in) :: a(..)
      integer(c_int16_t),     value, intent(in) :: t
      integer(c_signed_char), value, intent(in) :: k
      integer(c_size_t),      value, intent(in) :: e
      integer(c_size_t),      value, intent(in) :: n
    end subroutine check_tk_ar
  end interface

contains

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
    c_int, c_ptr, c_loc, c_associated

  use, intrinsic :: iso_c_binding, only: &
    c_ptr

  use :: isof_m, only:  &
    CFI_type_cptr

  use :: isof_m, only: &
    check_tk_as,       &
    check_tk_ar

  use :: isof_m, only: &
    cfi_encode_type
  
  implicit none

  integer                           :: i
  integer(kind=c_size_t), parameter :: b = 8
  integer,                parameter :: n = 11
  
  type, bind(c) :: c_foo_t
    integer(kind=c_int) :: a
  end type c_foo_t
  
  type(c_foo_t), parameter :: ref_c_foo_t(*) = [(c_foo_t(a=i), i=1,n)]
  
  type(c_foo_t), protected, target :: target_c_foo_t(n)

  
contains

  subroutine check_c_ptr()
    type(c_ptr) :: p(n)
    integer :: i
    !
    target_c_foo_t = ref_c_foo_t
    p = [(c_loc(target_c_foo_t(i)), i=1,n)]
    call f_check_c_ptr_as(p)
    if(any(target_c_foo_t(:)%a/=ref_c_foo_t(:)%a)) stop 1
    do i = 1, n
      if(.not.c_associated(p(i), c_loc(target_c_foo_t(i)))) stop 2
    end do
    target_c_foo_t = ref_c_foo_t
    p = [(c_loc(target_c_foo_t(i)), i=1,n)]
    call c_check_c_ptr_as(p)
    if(any(target_c_foo_t(:)%a/=ref_c_foo_t(:)%a)) stop 3
    do i = 1, n
      if(.not.c_associated(p(i), c_loc(target_c_foo_t(i)))) stop 4
    end do
    target_c_foo_t = ref_c_foo_t
    p = [(c_loc(target_c_foo_t(i)), i=1,n)]
    call f_check_c_ptr_ar(p)
    if(any(target_c_foo_t(:)%a/=ref_c_foo_t(:)%a)) stop 5
    do i = 1, n
      if(.not.c_associated(p(i), c_loc(target_c_foo_t(i)))) stop 6
    end do
    target_c_foo_t = ref_c_foo_t
    p = [(c_loc(target_c_foo_t(i)), i=1,n)]
    call c_check_c_ptr_ar(p)
    if(any(target_c_foo_t(:)%a/=ref_c_foo_t(:)%a)) stop 7
    do i = 1, n
      if(.not.c_associated(p(i), c_loc(target_c_foo_t(i)))) stop 8
    end do
    return
  end subroutine check_c_ptr

  subroutine f_check_c_ptr_as(a)
    type(c_ptr), intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e
    !
    k = 0
    e = storage_size(a)/b
    t = cfi_encode_type(CFI_type_cptr, k)
    ! Assumes 64-bit target.
    ! if(e/=8) stop 9
    if(any(target_c_foo_t(:)%a/=ref_c_foo_t(:)%a)) stop 10
    do i = 1, n
      if(.not.c_associated(a(i), c_loc(target_c_foo_t(i)))) stop 11
    end do
    call check_tk_as(a, t, k, e, 1_c_size_t)
    if(any(target_c_foo_t(:)%a/=ref_c_foo_t(:)%a)) stop 12
    do i = 1, n
      if(.not.c_associated(a(i), c_loc(target_c_foo_t(i)))) stop 13
    end do
    return
  end subroutine f_check_c_ptr_as

  subroutine c_check_c_ptr_as(a) bind(c)
    type(c_ptr), intent(in) :: a(:)
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e
    !
    k = 0
    e = storage_size(a)/b
    t = cfi_encode_type(CFI_type_cptr, k)
    ! Assumes 64-bit target.
    ! if(e/=8) stop 14
    if(any(target_c_foo_t(:)%a/=ref_c_foo_t(:)%a)) stop 15
    do i = 1, n
      if(.not.c_associated(a(i), c_loc(target_c_foo_t(i)))) stop 16
    end do
    call check_tk_as(a, t, k, e, 1_c_size_t)
    if(any(target_c_foo_t(:)%a/=ref_c_foo_t(:)%a)) stop 17
    do i = 1, n
      if(.not.c_associated(a(i), c_loc(target_c_foo_t(i)))) stop 18
    end do
    return
  end subroutine c_check_c_ptr_as

  subroutine f_check_c_ptr_ar(a)
    type(c_ptr), intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e
    !
    k = 0
    e = storage_size(a)/b
    t = cfi_encode_type(CFI_type_cptr, k)
    ! Assumes 64-bit target.
    ! if(e/=8) stop 19
    select rank(a)
    rank(1)
      if(any(target_c_foo_t(:)%a/=ref_c_foo_t(:)%a)) stop 20
      do i = 1, n
        if(.not.c_associated(a(i), c_loc(target_c_foo_t(i)))) stop 21
      end do
    rank default
      stop 22
    end select
    call check_tk_ar(a, t, k, e, 1_c_size_t)
    select rank(a)
    rank(1)
      if(any(target_c_foo_t(:)%a/=ref_c_foo_t(:)%a)) stop 23
      do i = 1, n
        if(.not.c_associated(a(i), c_loc(target_c_foo_t(i)))) stop 24
      end do
    rank default
      stop 25
    end select
    return
  end subroutine f_check_c_ptr_ar

  subroutine c_check_c_ptr_ar(a) bind(c)
    type(c_ptr), intent(in) :: a(..)
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e
    !
    k = 0
    e = storage_size(a)/b
    t = cfi_encode_type(CFI_type_cptr, k)
    ! Assumes 64-bit target.
    ! if(e/=8) stop 26
    select rank(a)
    rank(1)
      if(any(target_c_foo_t(:)%a/=ref_c_foo_t(:)%a)) stop 27
      do i = 1, n
        if(.not.c_associated(a(i), c_loc(target_c_foo_t(i)))) stop 28
      end do
    rank default
      stop 29
    end select
    call check_tk_ar(a, t, k, e, 1_c_size_t)
    select rank(a)
    rank(1)
      if(any(target_c_foo_t(:)%a/=ref_c_foo_t(:)%a)) stop 30
      do i = 1, n
        if(.not.c_associated(a(i), c_loc(target_c_foo_t(i)))) stop 31
      end do
    rank default
      stop 32
    end select
    return
  end subroutine c_check_c_ptr_ar

end module iso_check_m

program main_p
  
  use :: iso_check_m, only: &
    check_c_ptr

  implicit none

  call check_c_ptr()
  stop

end program main_p

!! Local Variables:
!! mode: f90
!! End:

