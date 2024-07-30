! Fails on x86 targets where sizeof(long double) == 16.
! { dg-do run }
! { dg-require-effective-target fortran_real_c_float128 }
! { dg-additional-sources PR100914.c }
! { dg-additional-options "-Wno-pedantic" }
!
! Test the fix for PR100914
! 

module isof_m

  use, intrinsic :: iso_c_binding, only: &
    c_signed_char, c_int16_t
  
  implicit none

  private
  
  public ::                       &
    CFI_type_Complex,             &
    CFI_type_float_Complex,       &
    CFI_type_double_Complex,      &
    CFI_type_long_double_Complex, &
    CFI_type_float128_Complex
 
  public ::      &
    check_tk_as, &
    check_tk_ar
  
  
  public ::          &
    cfi_encode_type
  
  integer, parameter :: CFI_type_t = c_int16_t
  
  integer(kind=c_int16_t), parameter :: CFI_type_mask = int(z"FF", kind=c_int16_t)
  integer(kind=c_int16_t), parameter :: CFI_type_kind_shift = 8_c_int16_t

  ! Intrinsic types. Their kind number defines their storage size. */
  integer(kind=c_signed_char), parameter :: CFI_type_Complex = 4

  ! C-Fortran Interoperability types.
  integer(kind=cfi_type_t), parameter :: CFI_type_float_Complex = &
    ior(int(CFI_type_Complex, kind=c_int16_t), shiftl(4_c_int16_t, CFI_type_kind_shift))
  integer(kind=cfi_type_t), parameter :: CFI_type_double_Complex = &
    ior(int(CFI_type_Complex, kind=c_int16_t), shiftl(8_c_int16_t, CFI_type_kind_shift))
  integer(kind=cfi_type_t), parameter :: CFI_type_long_double_Complex = &
    ior(int(CFI_type_Complex, kind=c_int16_t), shiftl(10_c_int16_t, CFI_type_kind_shift))
  integer(kind=cfi_type_t), parameter :: CFI_type_float128_Complex = &
    ior(int(CFI_type_Complex, kind=c_int16_t), shiftl(16_c_int16_t, CFI_type_kind_shift))

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
    c_float_complex,                      &
    c_double_complex,                     &
    c_long_double_complex,                &
    c_float128_complex

  use :: isof_m, only:  &
    CFI_type_Complex

  use :: isof_m, only:            &
    CFI_type_float_Complex,       &
    CFI_type_double_Complex,      &
    CFI_type_long_double_Complex, &
    CFI_type_float128_Complex

  use :: isof_m, only: &
    check_tk_as,       &
    check_tk_ar

  use :: isof_m, only: &
    cfi_encode_type
  
  implicit none

  private

  public ::                      &
    check_c_float_complex,       &
    check_c_double_complex,      &
    check_c_long_double_complex, &
    check_c_float128_complex    

  integer                           :: i
  integer(kind=c_size_t), parameter :: b = 8
  integer,                parameter :: n = 11  
  
  complex(kind=c_float_complex), parameter :: ref_c_float_complex(*)  = &
    [(cmplx(i, 2*i, kind=c_float_complex),  i=1,n)]
  complex(kind=c_double_complex), parameter :: ref_c_double_complex(*)  = &
    [(cmplx(i, 2*i, kind=c_double_complex),  i=1,n)]
  complex(kind=c_long_double_complex), parameter :: ref_c_long_double_complex(*)  = &
    [(cmplx(i, 2*i, kind=c_long_double_complex),  i=1,n)]
  complex(kind=c_float128_complex), parameter :: ref_c_float128_complex(*)  = &
    [(cmplx(i, 2*i, kind=c_float128_complex),  i=1,n)]
  
contains

  ! CFI_type_float_complex
  subroutine check_c_float_complex()
    complex(kind=c_float_complex) :: a(n)
    !
    if (c_float_complex/=4) stop 1
    a = ref_c_float_complex
    call f_check_c_float_complex_as(a)
    if(any(abs(a-ref_c_float_complex)>0.0_c_float_complex)) stop 2
    a = ref_c_float_complex
    call c_check_c_float_complex_as(a)
    if(any(abs(a-ref_c_float_complex)>0.0_c_float_complex)) stop 3
    a = ref_c_float_complex
    call f_check_c_float_complex_ar(a)
    if(any(abs(a-ref_c_float_complex)>0.0_c_float_complex)) stop 4
    a = ref_c_float_complex
    call c_check_c_float_complex_ar(a)
    if(any(abs(a-ref_c_float_complex)>0.0_c_float_complex)) stop 5
    return
  end subroutine check_c_float_complex

  subroutine f_check_c_float_complex_as(a)
    complex(kind=c_float_complex), intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e
    !
    k = kind(a)
    e = storage_size(a)/b
    t = cfi_encode_type(CFI_type_complex, k)
    if(k<=0_c_signed_char) stop 6
    if(k/=4_c_signed_char) stop 7
    if(int(k, kind=c_size_t)/=(e/2)) stop 8
    if(t/=CFI_type_float_complex) stop 9
    if(any(abs(a-ref_c_float_complex)>0.0_c_float_complex)) stop 10
    call check_tk_as(a, t, k, e, 1_c_size_t)
    if(any(abs(a-ref_c_float_complex)>0.0_c_float_complex)) stop 11
    return
  end subroutine f_check_c_float_complex_as

  subroutine c_check_c_float_complex_as(a) bind(c)
    complex(kind=c_float_complex), intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e
    !
    k = kind(a)
    e = storage_size(a)/b
    t = cfi_encode_type(CFI_type_complex, k)
    if(k<=0_c_signed_char) stop 12
    if(k/=4_c_signed_char) stop 13
    if(int(k, kind=c_size_t)/=(e/2)) stop 14
    if(t/=CFI_type_float_complex) stop 15
    if(any(abs(a-ref_c_float_complex)>0.0_c_float_complex)) stop 16
    call check_tk_as(a, t, k, e, 1_c_size_t)
    if(any(abs(a-ref_c_float_complex)>0.0_c_float_complex)) stop 17
    return
  end subroutine c_check_c_float_complex_as

  subroutine f_check_c_float_complex_ar(a)
    complex(kind=c_float_complex), intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e
    !
    k = kind(a)
    e = storage_size(a)/b
    t = cfi_encode_type(CFI_type_complex, k)
    if(k<=0_c_signed_char) stop 18
    if(k/=4_c_signed_char) stop 19
    if(int(k, kind=c_size_t)/=(e/2)) stop 20
    if(t/=CFI_type_float_complex) stop 21
    select rank(a)
    rank(1)
      if(any(abs(a-ref_c_float_complex)>0.0_c_float_complex)) stop 22
    rank default
      stop 23
    end select
    call check_tk_ar(a, t, k, e, 1_c_size_t)
    select rank(a)
    rank(1)
      if(any(abs(a-ref_c_float_complex)>0.0_c_float_complex)) stop 24
    rank default
      stop 25
    end select
    return
  end subroutine f_check_c_float_complex_ar

  subroutine c_check_c_float_complex_ar(a) bind(c)
    complex(kind=c_float_complex), intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e
    !
    k = kind(a)
    e = storage_size(a)/b
    t = cfi_encode_type(CFI_type_complex, k)
    if(k<=0_c_signed_char) stop 26
    if(k/=4_c_signed_char) stop 27
    if(int(k, kind=c_size_t)/=(e/2)) stop 28
    if(t/=CFI_type_float_complex) stop 29
    select rank(a)
    rank(1)
      if(any(abs(a-ref_c_float_complex)>0.0_c_float_complex)) stop 30
    rank default
      stop 31
    end select
    call check_tk_ar(a, t, k, e, 1_c_size_t)
    select rank(a)
    rank(1)
      if(any(abs(a-ref_c_float_complex)>0.0_c_float_complex)) stop 32
    rank default
      stop 33
    end select
    return
  end subroutine c_check_c_float_complex_ar

  ! CFI_type_double_complex
  subroutine check_c_double_complex()
    complex(kind=c_double_complex) :: a(n)
    !
    if (c_double_complex/=8) stop 34
    a = ref_c_double_complex
    call f_check_c_double_complex_as(a)
    if(any(abs(a-ref_c_double_complex)>0.0_c_double_complex)) stop 35
    a = ref_c_double_complex
    call c_check_c_double_complex_as(a)
    if(any(abs(a-ref_c_double_complex)>0.0_c_double_complex)) stop 36
    a = ref_c_double_complex
    call f_check_c_double_complex_ar(a)
    if(any(abs(a-ref_c_double_complex)>0.0_c_double_complex)) stop 37
    a = ref_c_double_complex
    call c_check_c_double_complex_ar(a)
    if(any(abs(a-ref_c_double_complex)>0.0_c_double_complex)) stop 38
    return
  end subroutine check_c_double_complex

  subroutine f_check_c_double_complex_as(a)
    complex(kind=c_double_complex), intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e
    !
    k = kind(a)
    e = storage_size(a)/b
    t = cfi_encode_type(CFI_type_complex, k)
    if(k<=0_c_signed_char) stop 39
    if(k/=8_c_signed_char) stop 40
    if(int(k, kind=c_size_t)/=(e/2)) stop 41
    if(t/=CFI_type_double_complex) stop 42
    if(any(abs(a-ref_c_double_complex)>0.0_c_double_complex)) stop 43
    call check_tk_as(a, t, k, e, 1_c_size_t)
    if(any(abs(a-ref_c_double_complex)>0.0_c_double_complex)) stop 44
    return
  end subroutine f_check_c_double_complex_as

  subroutine c_check_c_double_complex_as(a) bind(c)
    complex(kind=c_double_complex), intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e
    !
    k = kind(a)
    e = storage_size(a)/b
    t = cfi_encode_type(CFI_type_complex, k)
    if(k<=0_c_signed_char) stop 45
    if(k/=8_c_signed_char) stop 46
    if(int(k, kind=c_size_t)/=(e/2)) stop 47
    if(t/=CFI_type_double_complex) stop 48
    if(any(abs(a-ref_c_double_complex)>0.0_c_double_complex)) stop 49
    call check_tk_as(a, t, k, e, 1_c_size_t)
    if(any(abs(a-ref_c_double_complex)>0.0_c_double_complex)) stop 50
    return
  end subroutine c_check_c_double_complex_as

  subroutine f_check_c_double_complex_ar(a)
    complex(kind=c_double_complex), intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e
    !
    k = kind(a)
    e = storage_size(a)/b
    t = cfi_encode_type(CFI_type_complex, k)
    if(k<=0_c_signed_char) stop 51
    if(k/=8_c_signed_char) stop 52
    if(int(k, kind=c_size_t)/=(e/2)) stop 53
    if(t/=CFI_type_double_complex) stop 54
    select rank(a)
    rank(1)
      if(any(abs(a-ref_c_double_complex)>0.0_c_double_complex)) stop 55
    rank default
      stop 56
    end select
    call check_tk_ar(a, t, k, e, 1_c_size_t)
    select rank(a)
    rank(1)
      if(any(abs(a-ref_c_double_complex)>0.0_c_double_complex)) stop 57
    rank default
      stop 58
    end select
    return
  end subroutine f_check_c_double_complex_ar

  subroutine c_check_c_double_complex_ar(a) bind(c)
    complex(kind=c_double_complex), intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e
    !
    k = kind(a)
    e = storage_size(a)/b
    t = cfi_encode_type(CFI_type_complex, k)
    if(k<=0_c_signed_char) stop 59
    if(k/=8_c_signed_char) stop 60
    if(int(k, kind=c_size_t)/=(e/2)) stop 61
    if(t/=CFI_type_double_complex) stop 62
    select rank(a)
    rank(1)
      if(any(abs(a-ref_c_double_complex)>0.0_c_double_complex)) stop 63
    rank default
      stop 64
    end select
    call check_tk_ar(a, t, k, e, 1_c_size_t)
    select rank(a)
    rank(1)
      if(any(abs(a-ref_c_double_complex)>0.0_c_double_complex)) stop 65
    rank default
      stop 66
    end select
    return
  end subroutine c_check_c_double_complex_ar

  ! CFI_type_long_double_complex
  subroutine check_c_long_double_complex()
    complex(kind=c_long_double_complex) :: a(n)
    !
    if (c_long_double_complex/=10) stop 67
    a = ref_c_long_double_complex
    call f_check_c_long_double_complex_as(a)
    if(any(abs(a-ref_c_long_double_complex)>0.0_c_long_double_complex)) stop 68
    a = ref_c_long_double_complex
    call c_check_c_long_double_complex_as(a)
    if(any(abs(a-ref_c_long_double_complex)>0.0_c_long_double_complex)) stop 69
    a = ref_c_long_double_complex
    call f_check_c_long_double_complex_ar(a)
    if(any(abs(a-ref_c_long_double_complex)>0.0_c_long_double_complex)) stop 70
    a = ref_c_long_double_complex
    call c_check_c_long_double_complex_ar(a)
    if(any(abs(a-ref_c_long_double_complex)>0.0_c_long_double_complex)) stop 71
    return
  end subroutine check_c_long_double_complex

  subroutine f_check_c_long_double_complex_as(a)
    complex(kind=c_long_double_complex), intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e
    !
    k = kind(a)
    e = storage_size(a)/b
    t = cfi_encode_type(CFI_type_complex, k)
    if(k<=0_c_signed_char) stop 72
    if(k/=10_c_signed_char) stop 73
    if(e/=32) stop 74
    if(t/=CFI_type_long_double_complex) stop 75
    if(any(abs(a-ref_c_long_double_complex)>0.0_c_long_double_complex)) stop 76
    call check_tk_as(a, t, k, e, 1_c_size_t)
    if(any(abs(a-ref_c_long_double_complex)>0.0_c_long_double_complex)) stop 77
    return
  end subroutine f_check_c_long_double_complex_as

  subroutine c_check_c_long_double_complex_as(a) bind(c)
    complex(kind=c_long_double_complex), intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e
    !
    k = kind(a)
    e = storage_size(a)/b
    t = cfi_encode_type(CFI_type_complex, k)
    if(k<=0_c_signed_char) stop 78
    if(k/=10_c_signed_char) stop 79
    if(e/=32) stop 80
    if(t/=CFI_type_long_double_complex) stop 81
    if(any(abs(a-ref_c_long_double_complex)>0.0_c_long_double_complex)) stop 82
    call check_tk_as(a, t, k, e, 1_c_size_t)
    if(any(abs(a-ref_c_long_double_complex)>0.0_c_long_double_complex)) stop 83
    return
  end subroutine c_check_c_long_double_complex_as

  subroutine f_check_c_long_double_complex_ar(a)
    complex(kind=c_long_double_complex), intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e
    !
    k = kind(a)
    e = storage_size(a)/b
    t = cfi_encode_type(CFI_type_complex, k)
    if(k<=0_c_signed_char) stop 84
    if(k/=10_c_signed_char) stop 85
    if(e/=32) stop 86
    if(t/=CFI_type_long_double_complex) stop 87
    select rank(a)
    rank(1)
      if(any(abs(a-ref_c_long_double_complex)>0.0_c_long_double_complex)) stop 88
    rank default
      stop 89
    end select
    call check_tk_ar(a, t, k, e, 1_c_size_t)
    select rank(a)
    rank(1)
      if(any(abs(a-ref_c_long_double_complex)>0.0_c_long_double_complex)) stop 90
    rank default
      stop 91
    end select
    return
  end subroutine f_check_c_long_double_complex_ar

  subroutine c_check_c_long_double_complex_ar(a) bind(c)
    complex(kind=c_long_double_complex), intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e
    !
    k = kind(a)
    e = storage_size(a)/b
    t = cfi_encode_type(CFI_type_complex, k)
    if(k<=0_c_signed_char) stop 92
    if(k/=10_c_signed_char) stop 93
    if(e/=32) stop 94
    if(t/=CFI_type_long_double_complex) stop 95
    select rank(a)
    rank(1)
      if(any(abs(a-ref_c_long_double_complex)>0.0_c_long_double_complex)) stop 96
    rank default
      stop 97
    end select
    call check_tk_ar(a, t, k, e, 1_c_size_t)
    select rank(a)
    rank(1)
      if(any(abs(a-ref_c_long_double_complex)>0.0_c_long_double_complex)) stop 98
    rank default
      stop 99
    end select
    return
  end subroutine c_check_c_long_double_complex_ar

  ! CFI_type_float128_complex
  subroutine check_c_float128_complex()
    complex(kind=c_float128_complex) :: a(n)
    !
    if (c_float128_complex/=16) stop 100
    a = ref_c_float128_complex
    call f_check_c_float128_complex_as(a)
    if(any(abs(a-ref_c_float128_complex)>0.0_c_float128_complex)) stop 101
    a = ref_c_float128_complex
    call c_check_c_float128_complex_as(a)
    if(any(abs(a-ref_c_float128_complex)>0.0_c_float128_complex)) stop 102
    a = ref_c_float128_complex
    call f_check_c_float128_complex_ar(a)
    if(any(abs(a-ref_c_float128_complex)>0.0_c_float128_complex)) stop 103
    a = ref_c_float128_complex
    call c_check_c_float128_complex_ar(a)
    if(any(abs(a-ref_c_float128_complex)>0.0_c_float128_complex)) stop 104
    return
  end subroutine check_c_float128_complex

  subroutine f_check_c_float128_complex_as(a)
    complex(kind=c_float128_complex), intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e
    !
    k = kind(a)
    e = storage_size(a)/b
    t = cfi_encode_type(CFI_type_complex, k)
    if(k<=0_c_signed_char) stop 105
    if(k/=16_c_signed_char) stop 106
    if(int(k, kind=c_size_t)/=(e/2)) stop 107
    if(t/=CFI_type_float128_complex) stop 108
    if(any(abs(a-ref_c_float128_complex)>0.0_c_float128_complex)) stop 109
    call check_tk_as(a, t, k, e, 1_c_size_t)
    if(any(abs(a-ref_c_float128_complex)>0.0_c_float128_complex)) stop 110
    return
  end subroutine f_check_c_float128_complex_as

  subroutine c_check_c_float128_complex_as(a) bind(c)
    complex(kind=c_float128_complex), intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e
    !
    k = kind(a)
    e = storage_size(a)/b
    t = cfi_encode_type(CFI_type_complex, k)
    if(k<=0_c_signed_char) stop 111
    if(k/=16_c_signed_char) stop 112
    if(int(k, kind=c_size_t)/=(e/2)) stop 113
    if(t/=CFI_type_float128_complex) stop 114
    if(any(abs(a-ref_c_float128_complex)>0.0_c_float128_complex)) stop 115
    call check_tk_as(a, t, k, e, 1_c_size_t)
    if(any(abs(a-ref_c_float128_complex)>0.0_c_float128_complex)) stop 116
    return
  end subroutine c_check_c_float128_complex_as

  subroutine f_check_c_float128_complex_ar(a)
    complex(kind=c_float128_complex), intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e
    !
    k = kind(a)
    e = storage_size(a)/b
    t = cfi_encode_type(CFI_type_complex, k)
    if(k<=0_c_signed_char) stop 117
    if(k/=16_c_signed_char) stop 118
    if(int(k, kind=c_size_t)/=(e/2)) stop 119
    if(t/=CFI_type_float128_complex) stop 120
    select rank(a)
    rank(1)
      if(any(abs(a-ref_c_float128_complex)>0.0_c_float128_complex)) stop 121
    rank default
      stop 122
    end select
    call check_tk_ar(a, t, k, e, 1_c_size_t)
    select rank(a)
    rank(1)
      if(any(abs(a-ref_c_float128_complex)>0.0_c_float128_complex)) stop 123
    rank default
      stop 124
    end select
    return
  end subroutine f_check_c_float128_complex_ar

  subroutine c_check_c_float128_complex_ar(a) bind(c)
    complex(kind=c_float128_complex), intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e
    !
    k = kind(a)
    e = storage_size(a)/b
    t = cfi_encode_type(CFI_type_complex, k)
    if(k<=0_c_signed_char) stop 125
    if(k/=16_c_signed_char) stop 126
    if(int(k, kind=c_size_t)/=(e/2)) stop 127
    if(t/=CFI_type_float128_complex) stop 128
    select rank(a)
    rank(1)
      if(any(abs(a-ref_c_float128_complex)>0.0_c_float128_complex)) stop 129
    rank default
      stop 130
    end select
    call check_tk_ar(a, t, k, e, 1_c_size_t)
    select rank(a)
    rank(1)
      if(any(abs(a-ref_c_float128_complex)>0.0_c_float128_complex)) stop 131
    rank default
      stop 132
    end select
    return
  end subroutine c_check_c_float128_complex_ar

end module iso_check_m

program main_p
  
  use :: iso_check_m, only:      &
    check_c_float_complex,       &
    check_c_double_complex,      &
    check_c_long_double_complex, &
    check_c_float128_complex

  implicit none

  call check_c_float_complex()
  call check_c_double_complex()
  ! see PR100910
  ! call check_c_long_double_complex()
  call check_c_float128_complex()
  stop

end program main_p

!! Local Variables:
!! mode: f90
!! End:

