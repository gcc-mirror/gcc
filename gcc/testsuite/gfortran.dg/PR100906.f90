! { dg-do run }
! { dg-additional-sources PR100906.c }
!
! Test the fix for PR100906
! 

module isof_m

  use, intrinsic :: iso_c_binding, only: &
    c_signed_char, c_int16_t
  
  implicit none

  private
  
  public ::             &
    CFI_type_character

  public ::             &
    CFI_type_char,      &
    CFI_type_ucs4_char
 
  public ::      &
    check_tk_as, &
    check_tk_ar
  
  
  public ::          &
    cfi_encode_type
  
  integer, parameter :: CFI_type_t = c_int16_t
  
  integer(kind=c_int16_t), parameter :: CFI_type_mask = int(z"FF", kind=c_int16_t)
  integer(kind=c_int16_t), parameter :: CFI_type_kind_shift = 8_c_int16_t

  ! Intrinsic types. Their kind number defines their storage size. */
  integer(kind=c_signed_char), parameter :: CFI_type_Character = 5

  ! C-Fortran Interoperability types.
  integer(kind=cfi_type_t), parameter :: CFI_type_char      = &
    ior(int(CFI_type_Character, kind=c_int16_t), shiftl(1_c_int16_t, CFI_type_kind_shift))
  integer(kind=cfi_type_t), parameter :: CFI_type_ucs4_char = &
    ior(int(CFI_type_Character, kind=c_int16_t), shiftl(4_c_int16_t, CFI_type_kind_shift))

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
    c_char

  use :: isof_m, only:  &
    CFI_type_character

  use :: isof_m, only:  &
    CFI_type_char,      &
    CFI_type_ucs4_char

  use :: isof_m, only: &
    check_tk_as,       &
    check_tk_ar

  use :: isof_m, only: &
    cfi_encode_type

  implicit none

  private

  public ::               &
    check_c_char_l1,      &
    check_c_char_lm,      &
    check_c_ucs4_char_l1, &
    check_c_ucs4_char_lm
  
  integer                           :: i
  integer(kind=c_size_t), parameter :: b = 8
  integer,                parameter :: n = 11
  integer,                parameter :: m = 7
  
  integer, parameter :: c_ucs4_char = 4
  
  character(kind=c_char, len=1), parameter :: ref_c_char_l1(*) = &
    [(achar(i+iachar("A")-1, kind=c_char), i=1,n)]
  character(kind=c_char, len=m), parameter :: ref_c_char_lm(*) = &
    [(repeat(achar(i+iachar("A")-1, kind=c_char), m), i=1,n)]
  character(kind=c_ucs4_char, len=1), parameter :: ref_c_ucs4_char_l1(*) = &
    [(achar(i+iachar("A")-1, kind=c_ucs4_char), i=1,n)]
  character(kind=c_ucs4_char, len=m), parameter :: ref_c_ucs4_char_lm(*) = &
    [(repeat(achar(i+iachar("A")-1, kind=c_ucs4_char), m), i=1,n)]

contains

  subroutine check_c_char_l1()
    character(kind=c_char, len=1), target :: a(n)
    !
    character(kind=c_char, len=:), pointer :: p(:)
    !
    a = ref_c_char_l1
    call f_check_c_char_c1_as(a)
    if(any(a/=ref_c_char_l1)) stop 1
    a = ref_c_char_l1
    call c_check_c_char_c1_as(a)
    if(any(a/=ref_c_char_l1)) stop 2
    a = ref_c_char_l1
    call f_check_c_char_c1_ar(a)
    if(any(a/=ref_c_char_l1)) stop 3
    a = ref_c_char_l1
    call c_check_c_char_c1_ar(a)
    if(any(a/=ref_c_char_l1)) stop 4
    a = ref_c_char_l1
    call f_check_c_char_a1_as(a)
    if(any(a/=ref_c_char_l1)) stop 5
    a = ref_c_char_l1
    call c_check_c_char_a1_as(a)
    if(any(a/=ref_c_char_l1)) stop 6
    a = ref_c_char_l1
    call f_check_c_char_a1_ar(a)
    if(any(a/=ref_c_char_l1)) stop 7
    a = ref_c_char_l1
    call c_check_c_char_a1_ar(a)
    if(any(a/=ref_c_char_l1)) stop 8
    a = ref_c_char_l1
    p => a  
    call f_check_c_char_d1_as(p)
    if(.not.associated(p)) stop 9
    if(.not.associated(p, a)) stop 10
    if(any(p/=ref_c_char_l1)) stop 11
    if(any(a/=ref_c_char_l1)) stop 12
    a = ref_c_char_l1
    p => a  
    call c_check_c_char_d1_as(p)
    if(.not.associated(p)) stop 13
    if(.not.associated(p, a)) stop 14
    if(any(p/=ref_c_char_l1)) stop 15
    if(any(a/=ref_c_char_l1)) stop 16
    a = ref_c_char_l1
    p => a  
    call f_check_c_char_d1_ar(p)
    if(.not.associated(p)) stop 17
    if(.not.associated(p, a)) stop 18
    if(any(p/=ref_c_char_l1)) stop 19
    if(any(a/=ref_c_char_l1)) stop 20
    a = ref_c_char_l1
    p => a  
    call c_check_c_char_d1_ar(p)
    if(.not.associated(p)) stop 21
    if(.not.associated(p, a)) stop 22
    if(any(p/=ref_c_char_l1)) stop 23
    if(any(a/=ref_c_char_l1)) stop 24
    return
  end subroutine check_c_char_l1

  subroutine f_check_c_char_c1_as(a)
    character(kind=c_char, len=1), intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*1)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 25
    if(k/=1_c_signed_char) stop 26
    if(n/=1) stop 27
    if(int(k, kind=c_size_t)/=e) stop 28
    if(t/=CFI_type_char) stop 29
    if(any(a/=ref_c_char_l1)) stop 30
    call check_tk_as(a, t, k, e, n)
    if(any(a/=ref_c_char_l1)) stop 31
    return
  end subroutine f_check_c_char_c1_as

  subroutine c_check_c_char_c1_as(a) bind(c)
    character(kind=c_char, len=1), intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*1)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 32
    if(k/=1_c_signed_char) stop 33
    if(n/=1) stop 34
    if(int(k, kind=c_size_t)/=e) stop 35
    if(t/=CFI_type_char) stop 36
    if(any(a/=ref_c_char_l1)) stop 37
    call check_tk_as(a, t, k, e, n)
    if(any(a/=ref_c_char_l1)) stop 38
    return
  end subroutine c_check_c_char_c1_as

  subroutine f_check_c_char_c1_ar(a)
    character(kind=c_char, len=1), intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*1)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 39
    if(k/=1_c_signed_char) stop 40
    if(n/=1) stop 41
    if(int(k, kind=c_size_t)/=e) stop 42
    if(t/=CFI_type_char) stop 43
    select rank(a)
    rank(1)
      if(any(a/=ref_c_char_l1)) stop 44
    rank default
      stop 45
    end select
    call check_tk_ar(a, t, k, e, n)
    select rank(a)
    rank(1)
      if(any(a/=ref_c_char_l1)) stop 46
    rank default
      stop 47
    end select
    return
  end subroutine f_check_c_char_c1_ar

  subroutine c_check_c_char_c1_ar(a) bind(c)
    character(kind=c_char, len=1), intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*1)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 48
    if(k/=1_c_signed_char) stop 49
    if(n/=1) stop 50
    if(int(k, kind=c_size_t)/=e) stop 51
    if(t/=CFI_type_char) stop 52
    select rank(a)
    rank(1)
      if(any(a/=ref_c_char_l1)) stop 53
    rank default
      stop 54
    end select
    call check_tk_ar(a, t, k, e, n)
    select rank(a)
    rank(1)
      if(any(a/=ref_c_char_l1)) stop 55
    rank default
      stop 56
    end select
    return
  end subroutine c_check_c_char_c1_ar

  subroutine f_check_c_char_a1_as(a)
    character(kind=c_char, len=*), intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*1)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 57
    if(k/=1_c_signed_char) stop 58
    if(n/=1) stop 59
    if(int(k, kind=c_size_t)/=e) stop 60
    if(t/=CFI_type_char) stop 61
    if(any(a/=ref_c_char_l1)) stop 62
    call check_tk_as(a, t, k, e, n)
    if(any(a/=ref_c_char_l1)) stop 63
    return
  end subroutine f_check_c_char_a1_as

  subroutine c_check_c_char_a1_as(a) bind(c)
    character(kind=c_char, len=*), intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*1)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 64
    if(k/=1_c_signed_char) stop 65
    if(n/=1) stop 66
    if(int(k, kind=c_size_t)/=e) stop 67
    if(t/=CFI_type_char) stop 68
    if(any(a/=ref_c_char_l1)) stop 69
    call check_tk_as(a, t, k, e, n)
    if(any(a/=ref_c_char_l1)) stop 70
    return
  end subroutine c_check_c_char_a1_as

  subroutine f_check_c_char_a1_ar(a)
    character(kind=c_char, len=*), intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*1)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 71
    if(k/=1_c_signed_char) stop 72
    if(n/=1) stop 73
    if(int(k, kind=c_size_t)/=e) stop 74
    if(t/=CFI_type_char) stop 75
    select rank(a)
    rank(1)
      if(any(a/=ref_c_char_l1)) stop 76
    rank default
      stop 77
    end select
    call check_tk_ar(a, t, k, e, n)
    select rank(a)
    rank(1)
      if(any(a/=ref_c_char_l1)) stop 78
    rank default
      stop 79
    end select
    return
  end subroutine f_check_c_char_a1_ar

  subroutine c_check_c_char_a1_ar(a) bind(c)
    character(kind=c_char, len=*), intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*1)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 80
    if(k/=1_c_signed_char) stop 81
    if(n/=1) stop 82
    if(int(k, kind=c_size_t)/=e) stop 83
    if(t/=CFI_type_char) stop 84
    select rank(a)
    rank(1)
      if(any(a/=ref_c_char_l1)) stop 85
    rank default
      stop 86
    end select
    call check_tk_ar(a, t, k, e, n)
    select rank(a)
    rank(1)
      if(any(a/=ref_c_char_l1)) stop 87
    rank default
      stop 88
    end select
    return
  end subroutine c_check_c_char_a1_ar

  subroutine f_check_c_char_d1_as(a)
    character(kind=c_char, len=:), pointer, intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*1)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 89
    if(k/=1_c_signed_char) stop 90
    if(n/=1) stop 91
    if(int(k, kind=c_size_t)/=e) stop 92
    if(t/=CFI_type_char) stop 93
    if(any(a/=ref_c_char_l1)) stop 94
    call check_tk_as(a, t, k, e, n)
    if(any(a/=ref_c_char_l1)) stop 95
    return
  end subroutine f_check_c_char_d1_as

  subroutine c_check_c_char_d1_as(a) bind(c)
    character(kind=c_char, len=:), pointer, intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*1)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 96
    if(k/=1_c_signed_char) stop 97
    if(n/=1) stop 98
    if(int(k, kind=c_size_t)/=e) stop 99
    if(t/=CFI_type_char) stop 100
    if(any(a/=ref_c_char_l1)) stop 101
    call check_tk_as(a, t, k, e, n)
    if(any(a/=ref_c_char_l1)) stop 102
    return
  end subroutine c_check_c_char_d1_as

  subroutine f_check_c_char_d1_ar(a)
    character(kind=c_char, len=:), pointer, intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*1)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 103
    if(k/=1_c_signed_char) stop 104
    if(n/=1) stop 105
    if(int(k, kind=c_size_t)/=e) stop 106
    if(t/=CFI_type_char) stop 107
    select rank(a)
    rank(1)
      if(any(a/=ref_c_char_l1)) stop 108
    rank default
      stop 109
    end select
    call check_tk_ar(a, t, k, e, n)
    select rank(a)
    rank(1)
      if(any(a/=ref_c_char_l1)) stop 110
    rank default
      stop 111
    end select
    return
  end subroutine f_check_c_char_d1_ar

  subroutine c_check_c_char_d1_ar(a) bind(c)
    character(kind=c_char, len=:), pointer, intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*1)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 112
    if(k/=1_c_signed_char) stop 113
    if(n/=1) stop 114
    if(int(k, kind=c_size_t)/=e) stop 115
    if(t/=CFI_type_char) stop 116
    select rank(a)
    rank(1)
      if(any(a/=ref_c_char_l1)) stop 117
    rank default
      stop 118
    end select
    call check_tk_ar(a, t, k, e, n)
    select rank(a)
    rank(1)
      if(any(a/=ref_c_char_l1)) stop 119
    rank default
      stop 120
    end select
    return
  end subroutine c_check_c_char_d1_ar

  subroutine check_c_char_lm()
    character(kind=c_char, len=m), target :: a(n)
    !
    character(kind=c_char, len=:), pointer :: p(:)
    !
    a = ref_c_char_lm
    call f_check_c_char_cm_as(a)
    if(any(a/=ref_c_char_lm)) stop 121
    a = ref_c_char_lm
    call c_check_c_char_cm_as(a)
    if(any(a/=ref_c_char_lm)) stop 122
    a = ref_c_char_lm
    call f_check_c_char_cm_ar(a)
    if(any(a/=ref_c_char_lm)) stop 123
    a = ref_c_char_lm
    call c_check_c_char_cm_ar(a)
    if(any(a/=ref_c_char_lm)) stop 124
    a = ref_c_char_lm
    call f_check_c_char_am_as(a)
    if(any(a/=ref_c_char_lm)) stop 125
    a = ref_c_char_lm
    call c_check_c_char_am_as(a)
    if(any(a/=ref_c_char_lm)) stop 126
    a = ref_c_char_lm
    call f_check_c_char_am_ar(a)
    if(any(a/=ref_c_char_lm)) stop 127
    a = ref_c_char_lm
    call c_check_c_char_am_ar(a)
    if(any(a/=ref_c_char_lm)) stop 128
    a = ref_c_char_lm
    p => a  
    call f_check_c_char_dm_as(p)
    if(.not.associated(p)) stop 129
    if(.not.associated(p, a)) stop 130
    if(any(p/=ref_c_char_lm)) stop 131
    if(any(a/=ref_c_char_lm)) stop 132
    a = ref_c_char_lm
    p => a  
    call c_check_c_char_dm_as(p)
    if(.not.associated(p)) stop 133
    if(.not.associated(p, a)) stop 134
    if(any(p/=ref_c_char_lm)) stop 135
    if(any(a/=ref_c_char_lm)) stop 136
    a = ref_c_char_lm
    p => a  
    call f_check_c_char_dm_ar(p)
    if(.not.associated(p)) stop 137
    if(.not.associated(p, a)) stop 138
    if(any(p/=ref_c_char_lm)) stop 139
    if(any(a/=ref_c_char_lm)) stop 140
    a = ref_c_char_lm
    p => a  
    call c_check_c_char_dm_ar(p)
    if(.not.associated(p)) stop 141
    if(.not.associated(p, a)) stop 142
    if(any(p/=ref_c_char_lm)) stop 143
    if(any(a/=ref_c_char_lm)) stop 144
    return
  end subroutine check_c_char_lm

  subroutine f_check_c_char_cm_as(a)
    character(kind=c_char, len=m), intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*m)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 145
    if(k/=1_c_signed_char) stop 146
    if(n/=m) stop 147
    if(int(k, kind=c_size_t)/=e) stop 148
    if(t/=CFI_type_char) stop 149
    if(any(a/=ref_c_char_lm)) stop 150
    call check_tk_as(a, t, k, e, n)
    if(any(a/=ref_c_char_lm)) stop 151
    return
  end subroutine f_check_c_char_cm_as

  subroutine c_check_c_char_cm_as(a) bind(c)
    character(kind=c_char, len=m), intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*m)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 152
    if(k/=1_c_signed_char) stop 153
    if(n/=m) stop 154
    if(int(k, kind=c_size_t)/=e) stop 155
    if(t/=CFI_type_char) stop 156
    if(any(a/=ref_c_char_lm)) stop 157
    call check_tk_as(a, t, k, e, n)
    if(any(a/=ref_c_char_lm)) stop 158
    return
  end subroutine c_check_c_char_cm_as

  subroutine f_check_c_char_cm_ar(a)
    character(kind=c_char, len=m), intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*m)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 159
    if(k/=1_c_signed_char) stop 160
    if(n/=m) stop 161
    if(int(k, kind=c_size_t)/=e) stop 162
    if(t/=CFI_type_char) stop 163
    select rank(a)
    rank(1)
      if(any(a/=ref_c_char_lm)) stop 164
    rank default
      stop 165
    end select
    call check_tk_ar(a, t, k, e, n)
    select rank(a)
    rank(1)
      if(any(a/=ref_c_char_lm)) stop 166
    rank default
      stop 167
    end select
    return
  end subroutine f_check_c_char_cm_ar

  subroutine c_check_c_char_cm_ar(a) bind(c)
    character(kind=c_char, len=m), intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*m)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 168
    if(k/=1_c_signed_char) stop 169
    if(n/=m) stop 170
    if(int(k, kind=c_size_t)/=e) stop 171
    if(t/=CFI_type_char) stop 172
    select rank(a)
    rank(1)
      if(any(a/=ref_c_char_lm)) stop 173
    rank default
      stop 174
    end select
    call check_tk_ar(a, t, k, e, n)
    select rank(a)
    rank(1)
      if(any(a/=ref_c_char_lm)) stop 175
    rank default
      stop 176
    end select
    return
  end subroutine c_check_c_char_cm_ar

  subroutine f_check_c_char_am_as(a)
    character(kind=c_char, len=*), intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*m)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 177
    if(k/=1_c_signed_char) stop 178
    if(n/=m) stop 179
    if(int(k, kind=c_size_t)/=e) stop 180
    if(t/=CFI_type_char) stop 181
    if(any(a/=ref_c_char_lm)) stop 182
    call check_tk_as(a, t, k, e, n)
    if(any(a/=ref_c_char_lm)) stop 183
    return
  end subroutine f_check_c_char_am_as

  subroutine c_check_c_char_am_as(a) bind(c)
    character(kind=c_char, len=*), intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*m)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 184
    if(k/=1_c_signed_char) stop 185
    if(n/=m) stop 186
    if(int(k, kind=c_size_t)/=e) stop 187
    if(t/=CFI_type_char) stop 188
    if(any(a/=ref_c_char_lm)) stop 189
    call check_tk_as(a, t, k, e, n)
    if(any(a/=ref_c_char_lm)) stop 190
    return
  end subroutine c_check_c_char_am_as

  subroutine f_check_c_char_am_ar(a)
    character(kind=c_char, len=*), intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*m)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 191
    if(k/=1_c_signed_char) stop 192
    if(n/=m) stop 193
    if(int(k, kind=c_size_t)/=e) stop 194
    if(t/=CFI_type_char) stop 195
    select rank(a)
    rank(1)
      if(any(a/=ref_c_char_lm)) stop 196
    rank default
      stop 197
    end select
    call check_tk_ar(a, t, k, e, n)
    select rank(a)
    rank(1)
      if(any(a/=ref_c_char_lm)) stop 198
    rank default
      stop 199
    end select
    return
  end subroutine f_check_c_char_am_ar

  subroutine c_check_c_char_am_ar(a) bind(c)
    character(kind=c_char, len=*), intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*m)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 200
    if(k/=1_c_signed_char) stop 201
    if(n/=m) stop 202
    if(int(k, kind=c_size_t)/=e) stop 203
    if(t/=CFI_type_char) stop 204
    select rank(a)
    rank(1)
      if(any(a/=ref_c_char_lm)) stop 205
    rank default
      stop 206
    end select
    call check_tk_ar(a, t, k, e, n)
    select rank(a)
    rank(1)
      if(any(a/=ref_c_char_lm)) stop 207
    rank default
      stop 208
    end select
    return
  end subroutine c_check_c_char_am_ar

  subroutine f_check_c_char_dm_as(a)
    character(kind=c_char, len=:), pointer, intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*m)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 209
    if(k/=1_c_signed_char) stop 210
    if(n/=m) stop 211
    if(int(k, kind=c_size_t)/=e) stop 212
    if(t/=CFI_type_char) stop 213
    if(any(a/=ref_c_char_lm)) stop 214
    call check_tk_as(a, t, k, e, n)
    if(any(a/=ref_c_char_lm)) stop 215
    return
  end subroutine f_check_c_char_dm_as

  subroutine c_check_c_char_dm_as(a) bind(c)
    character(kind=c_char, len=:), pointer, intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*m)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 216
    if(k/=1_c_signed_char) stop 217
    if(n/=m) stop 218
    if(int(k, kind=c_size_t)/=e) stop 219
    if(t/=CFI_type_char) stop 220
    if(any(a/=ref_c_char_lm)) stop 221
    call check_tk_as(a, t, k, e, n)
    if(any(a/=ref_c_char_lm)) stop 222
    return
  end subroutine c_check_c_char_dm_as

  subroutine f_check_c_char_dm_ar(a)
    character(kind=c_char, len=:), pointer, intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*m)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 223
    if(k/=1_c_signed_char) stop 224
    if(n/=m) stop 225
    if(int(k, kind=c_size_t)/=e) stop 226
    if(t/=CFI_type_char) stop 227
    select rank(a)
    rank(1)
      if(any(a/=ref_c_char_lm)) stop 228
    rank default
      stop 229
    end select
    call check_tk_ar(a, t, k, e, n)
    select rank(a)
    rank(1)
      if(any(a/=ref_c_char_lm)) stop 230
    rank default
      stop 231
    end select
    return
  end subroutine f_check_c_char_dm_ar

  subroutine c_check_c_char_dm_ar(a) bind(c)
    character(kind=c_char, len=:), pointer, intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*m)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 232
    if(k/=1_c_signed_char) stop 233
    if(n/=m) stop 234
    if(int(k, kind=c_size_t)/=e) stop 235
    if(t/=CFI_type_char) stop 236
    select rank(a)
    rank(1)
      if(any(a/=ref_c_char_lm)) stop 237
    rank default
      stop 238
    end select
    call check_tk_ar(a, t, k, e, n)
    select rank(a)
    rank(1)
      if(any(a/=ref_c_char_lm)) stop 239
    rank default
      stop 240
    end select
    return
  end subroutine c_check_c_char_dm_ar

  subroutine check_c_ucs4_char_l1()
    character(kind=c_ucs4_char, len=1), target :: a(n)
    !
    character(kind=c_ucs4_char, len=:), pointer :: p(:)
    !
    a = ref_c_ucs4_char_l1
    call f_check_c_ucs4_char_c1_as(a)
    if(any(a/=ref_c_ucs4_char_l1)) stop 241
    a = ref_c_ucs4_char_l1
    call c_check_c_ucs4_char_c1_as(a)
    if(any(a/=ref_c_ucs4_char_l1)) stop 242
    a = ref_c_ucs4_char_l1
    call f_check_c_ucs4_char_c1_ar(a)
    if(any(a/=ref_c_ucs4_char_l1)) stop 243
    a = ref_c_ucs4_char_l1
    call c_check_c_ucs4_char_c1_ar(a)
    if(any(a/=ref_c_ucs4_char_l1)) stop 244
    a = ref_c_ucs4_char_l1
    call f_check_c_ucs4_char_a1_as(a)
    if(any(a/=ref_c_ucs4_char_l1)) stop 245
    a = ref_c_ucs4_char_l1
    call c_check_c_ucs4_char_a1_as(a)
    if(any(a/=ref_c_ucs4_char_l1)) stop 246
    a = ref_c_ucs4_char_l1
    call f_check_c_ucs4_char_a1_ar(a)
    if(any(a/=ref_c_ucs4_char_l1)) stop 247
    a = ref_c_ucs4_char_l1
    call c_check_c_ucs4_char_a1_ar(a)
    if(any(a/=ref_c_ucs4_char_l1)) stop 248
    a = ref_c_ucs4_char_l1
    p => a  
    call f_check_c_ucs4_char_d1_as(p)
    if(.not.associated(p)) stop 249
    if(.not.associated(p, a)) stop 250
    if(any(p/=ref_c_ucs4_char_l1)) stop 251
    if(any(a/=ref_c_ucs4_char_l1)) stop 252
    a = ref_c_ucs4_char_l1
    p => a  
    call c_check_c_ucs4_char_d1_as(p)
    if(.not.associated(p)) stop 253
    if(.not.associated(p, a)) stop 254
    if(any(p/=ref_c_ucs4_char_l1)) stop 255
    if(any(a/=ref_c_ucs4_char_l1)) stop 256
    a = ref_c_ucs4_char_l1
    p => a  
    call f_check_c_ucs4_char_d1_ar(p)
    if(.not.associated(p)) stop 257
    if(.not.associated(p, a)) stop 258
    if(any(p/=ref_c_ucs4_char_l1)) stop 259
    if(any(a/=ref_c_ucs4_char_l1)) stop 260
    a = ref_c_ucs4_char_l1
    p => a  
    call c_check_c_ucs4_char_d1_ar(p)
    if(.not.associated(p)) stop 261
    if(.not.associated(p, a)) stop 262
    if(any(p/=ref_c_ucs4_char_l1)) stop 263
    if(any(a/=ref_c_ucs4_char_l1)) stop 264
    return
  end subroutine check_c_ucs4_char_l1

  subroutine f_check_c_ucs4_char_c1_as(a)
    character(kind=c_ucs4_char, len=1), intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*1)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 265
    if(k/=4_c_signed_char) stop 266
    if(n/=1) stop 267
    if(int(k, kind=c_size_t)/=e) stop 268
    if(t/=CFI_type_ucs4_char) stop 269
    if(any(a/=ref_c_ucs4_char_l1)) stop 270
    call check_tk_as(a, t, k, e, n)
    if(any(a/=ref_c_ucs4_char_l1)) stop 271
    return
  end subroutine f_check_c_ucs4_char_c1_as

  subroutine c_check_c_ucs4_char_c1_as(a) bind(c)
    character(kind=c_ucs4_char, len=1), intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*1)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 272
    if(k/=4_c_signed_char) stop 273
    if(n/=1) stop 274
    if(int(k, kind=c_size_t)/=e) stop 275
    if(t/=CFI_type_ucs4_char) stop 276
    if(any(a/=ref_c_ucs4_char_l1)) stop 277
    call check_tk_as(a, t, k, e, n)
    if(any(a/=ref_c_ucs4_char_l1)) stop 278
    return
  end subroutine c_check_c_ucs4_char_c1_as

  subroutine f_check_c_ucs4_char_c1_ar(a)
    character(kind=c_ucs4_char, len=1), intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*1)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 279
    if(k/=4_c_signed_char) stop 280
    if(n/=1) stop 281
    if(int(k, kind=c_size_t)/=e) stop 282
    if(t/=CFI_type_ucs4_char) stop 283
    select rank(a)
    rank(1)
      if(any(a/=ref_c_ucs4_char_l1)) stop 284
    rank default
      stop 285
    end select
    call check_tk_ar(a, t, k, e, n)
    select rank(a)
    rank(1)
      if(any(a/=ref_c_ucs4_char_l1)) stop 286
    rank default
      stop 287
    end select
    return
  end subroutine f_check_c_ucs4_char_c1_ar

  subroutine c_check_c_ucs4_char_c1_ar(a) bind(c)
    character(kind=c_ucs4_char, len=1), intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*1)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 288
    if(k/=4_c_signed_char) stop 289
    if(n/=1) stop 290
    if(int(k, kind=c_size_t)/=e) stop 291
    if(t/=CFI_type_ucs4_char) stop 292
    select rank(a)
    rank(1)
      if(any(a/=ref_c_ucs4_char_l1)) stop 293
    rank default
      stop 294
    end select
    call check_tk_ar(a, t, k, e, n)
    select rank(a)
    rank(1)
      if(any(a/=ref_c_ucs4_char_l1)) stop 295
    rank default
      stop 296
    end select
    return
  end subroutine c_check_c_ucs4_char_c1_ar

  subroutine f_check_c_ucs4_char_a1_as(a)
    character(kind=c_ucs4_char, len=*), intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*1)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 297
    if(k/=4_c_signed_char) stop 298
    if(n/=1) stop 299
    if(int(k, kind=c_size_t)/=e) stop 300
    if(t/=CFI_type_ucs4_char) stop 301
    if(any(a/=ref_c_ucs4_char_l1)) stop 302
    call check_tk_as(a, t, k, e, n)
    if(any(a/=ref_c_ucs4_char_l1)) stop 303
    return
  end subroutine f_check_c_ucs4_char_a1_as

  subroutine c_check_c_ucs4_char_a1_as(a) bind(c)
    character(kind=c_ucs4_char, len=*), intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*1)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 304
    if(k/=4_c_signed_char) stop 305
    if(n/=1) stop 306
    if(int(k, kind=c_size_t)/=e) stop 307
    if(t/=CFI_type_ucs4_char) stop 308
    if(any(a/=ref_c_ucs4_char_l1)) stop 309
    call check_tk_as(a, t, k, e, n)
    if(any(a/=ref_c_ucs4_char_l1)) stop 310
    return
  end subroutine c_check_c_ucs4_char_a1_as

  subroutine f_check_c_ucs4_char_a1_ar(a)
    character(kind=c_ucs4_char, len=*), intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*1)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 311
    if(k/=4_c_signed_char) stop 312
    if(n/=1) stop 313
    if(int(k, kind=c_size_t)/=e) stop 314
    if(t/=CFI_type_ucs4_char) stop 315
    select rank(a)
    rank(1)
      if(any(a/=ref_c_ucs4_char_l1)) stop 316
    rank default
      stop 317
    end select
    call check_tk_ar(a, t, k, e, n)
    select rank(a)
    rank(1)
      if(any(a/=ref_c_ucs4_char_l1)) stop 318
    rank default
      stop 319
    end select
    return
  end subroutine f_check_c_ucs4_char_a1_ar

  subroutine c_check_c_ucs4_char_a1_ar(a) bind(c)
    character(kind=c_ucs4_char, len=*), intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*1)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 320
    if(k/=4_c_signed_char) stop 321
    if(n/=1) stop 322
    if(int(k, kind=c_size_t)/=e) stop 323
    if(t/=CFI_type_ucs4_char) stop 324
    select rank(a)
    rank(1)
      if(any(a/=ref_c_ucs4_char_l1)) stop 325
    rank default
      stop 326
    end select
    call check_tk_ar(a, t, k, e, n)
    select rank(a)
    rank(1)
      if(any(a/=ref_c_ucs4_char_l1)) stop 327
    rank default
      stop 328
    end select
    return
  end subroutine c_check_c_ucs4_char_a1_ar

  subroutine f_check_c_ucs4_char_d1_as(a)
    character(kind=c_ucs4_char, len=:), pointer, intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*1)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 329
    if(k/=4_c_signed_char) stop 330
    if(n/=1) stop 331
    if(int(k, kind=c_size_t)/=e) stop 332
    if(t/=CFI_type_ucs4_char) stop 333
    if(any(a/=ref_c_ucs4_char_l1)) stop 334
    call check_tk_as(a, t, k, e, n)
    if(any(a/=ref_c_ucs4_char_l1)) stop 335
    return
  end subroutine f_check_c_ucs4_char_d1_as

  subroutine c_check_c_ucs4_char_d1_as(a) bind(c)
    character(kind=c_ucs4_char, len=:), pointer, intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*1)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 336
    if(k/=4_c_signed_char) stop 337
    if(n/=1) stop 338
    if(int(k, kind=c_size_t)/=e) stop 339
    if(t/=CFI_type_ucs4_char) stop 340
    if(any(a/=ref_c_ucs4_char_l1)) stop 341
    call check_tk_as(a, t, k, e, n)
    if(any(a/=ref_c_ucs4_char_l1)) stop 342
    return
  end subroutine c_check_c_ucs4_char_d1_as

  subroutine f_check_c_ucs4_char_d1_ar(a)
    character(kind=c_ucs4_char, len=:), pointer, intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*1)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 343
    if(k/=4_c_signed_char) stop 344
    if(n/=1) stop 345
    if(int(k, kind=c_size_t)/=e) stop 346
    if(t/=CFI_type_ucs4_char) stop 347
    select rank(a)
    rank(1)
      if(any(a/=ref_c_ucs4_char_l1)) stop 348
    rank default
      stop 349
    end select
    call check_tk_ar(a, t, k, e, n)
    select rank(a)
    rank(1)
      if(any(a/=ref_c_ucs4_char_l1)) stop 350
    rank default
      stop 351
    end select
    return
  end subroutine f_check_c_ucs4_char_d1_ar

  subroutine c_check_c_ucs4_char_d1_ar(a) bind(c)
    character(kind=c_ucs4_char, len=:), pointer, intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*1)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 352
    if(k/=4_c_signed_char) stop 353
    if(n/=1) stop 354
    if(int(k, kind=c_size_t)/=e) stop 355
    if(t/=CFI_type_ucs4_char) stop 356
    select rank(a)
    rank(1)
      if(any(a/=ref_c_ucs4_char_l1)) stop 357
    rank default
      stop 358
    end select
    call check_tk_ar(a, t, k, e, n)
    select rank(a)
    rank(1)
      if(any(a/=ref_c_ucs4_char_l1)) stop 359
    rank default
      stop 360
    end select
    return
  end subroutine c_check_c_ucs4_char_d1_ar

  subroutine check_c_ucs4_char_lm()
    character(kind=c_ucs4_char, len=m), target :: a(n)
    !
    character(kind=c_ucs4_char, len=:), pointer :: p(:)
    !
    a = ref_c_ucs4_char_lm
    call f_check_c_ucs4_char_cm_as(a)
    if(any(a/=ref_c_ucs4_char_lm)) stop 361
    a = ref_c_ucs4_char_lm
    call c_check_c_ucs4_char_cm_as(a)
    if(any(a/=ref_c_ucs4_char_lm)) stop 362
    a = ref_c_ucs4_char_lm
    call f_check_c_ucs4_char_cm_ar(a)
    if(any(a/=ref_c_ucs4_char_lm)) stop 363
    a = ref_c_ucs4_char_lm
    call c_check_c_ucs4_char_cm_ar(a)
    if(any(a/=ref_c_ucs4_char_lm)) stop 364
    a = ref_c_ucs4_char_lm
    call f_check_c_ucs4_char_am_as(a)
    if(any(a/=ref_c_ucs4_char_lm)) stop 365
    a = ref_c_ucs4_char_lm
    call c_check_c_ucs4_char_am_as(a)
    if(any(a/=ref_c_ucs4_char_lm)) stop 366
    a = ref_c_ucs4_char_lm
    call f_check_c_ucs4_char_am_ar(a)
    if(any(a/=ref_c_ucs4_char_lm)) stop 367
    a = ref_c_ucs4_char_lm
    call c_check_c_ucs4_char_am_ar(a)
    if(any(a/=ref_c_ucs4_char_lm)) stop 368
    a = ref_c_ucs4_char_lm
    p => a  
    call f_check_c_ucs4_char_dm_as(p)
    if(.not.associated(p)) stop 369
    if(.not.associated(p, a)) stop 370
    if(any(p/=ref_c_ucs4_char_lm)) stop 371
    if(any(a/=ref_c_ucs4_char_lm)) stop 372
    a = ref_c_ucs4_char_lm
    p => a  
    call c_check_c_ucs4_char_dm_as(p)
    if(.not.associated(p)) stop 373
    if(.not.associated(p, a)) stop 374
    if(any(p/=ref_c_ucs4_char_lm)) stop 375
    if(any(a/=ref_c_ucs4_char_lm)) stop 376
    a = ref_c_ucs4_char_lm
    p => a  
    call f_check_c_ucs4_char_dm_ar(p)
    if(.not.associated(p)) stop 377
    if(.not.associated(p, a)) stop 378
    if(any(p/=ref_c_ucs4_char_lm)) stop 379
    if(any(a/=ref_c_ucs4_char_lm)) stop 380
    a = ref_c_ucs4_char_lm
    p => a  
    call c_check_c_ucs4_char_dm_ar(p)
    if(.not.associated(p)) stop 381
    if(.not.associated(p, a)) stop 382
    if(any(p/=ref_c_ucs4_char_lm)) stop 383
    if(any(a/=ref_c_ucs4_char_lm)) stop 384
    return
  end subroutine check_c_ucs4_char_lm

  subroutine f_check_c_ucs4_char_cm_as(a)
    character(kind=c_ucs4_char, len=m), intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*m)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 385
    if(k/=4_c_signed_char) stop 386
    if(n/=m) stop 387
    if(int(k, kind=c_size_t)/=e) stop 388
    if(t/=CFI_type_ucs4_char) stop 389
    if(any(a/=ref_c_ucs4_char_lm)) stop 390
    call check_tk_as(a, t, k, e, n)
    if(any(a/=ref_c_ucs4_char_lm)) stop 391
    return
  end subroutine f_check_c_ucs4_char_cm_as

  subroutine c_check_c_ucs4_char_cm_as(a) bind(c)
    character(kind=c_ucs4_char, len=m), intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*m)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 392
    if(k/=4_c_signed_char) stop 393
    if(n/=m) stop 394
    if(int(k, kind=c_size_t)/=e) stop 395
    if(t/=CFI_type_ucs4_char) stop 396
    if(any(a/=ref_c_ucs4_char_lm)) stop 397
    call check_tk_as(a, t, k, e, n)
    if(any(a/=ref_c_ucs4_char_lm)) stop 398
    return
  end subroutine c_check_c_ucs4_char_cm_as

  subroutine f_check_c_ucs4_char_cm_ar(a)
    character(kind=c_ucs4_char, len=m), intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*m)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 399
    if(k/=4_c_signed_char) stop 400
    if(n/=m) stop 401
    if(int(k, kind=c_size_t)/=e) stop 402
    if(t/=CFI_type_ucs4_char) stop 403
    select rank(a)
    rank(1)
      if(any(a/=ref_c_ucs4_char_lm)) stop 404
    rank default
      stop 405
    end select
    call check_tk_ar(a, t, k, e, n)
    select rank(a)
    rank(1)
      if(any(a/=ref_c_ucs4_char_lm)) stop 406
    rank default
      stop 407
    end select
    return
  end subroutine f_check_c_ucs4_char_cm_ar

  subroutine c_check_c_ucs4_char_cm_ar(a) bind(c)
    character(kind=c_ucs4_char, len=m), intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*m)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 408
    if(k/=4_c_signed_char) stop 409
    if(n/=m) stop 410
    if(int(k, kind=c_size_t)/=e) stop 411
    if(t/=CFI_type_ucs4_char) stop 412
    select rank(a)
    rank(1)
      if(any(a/=ref_c_ucs4_char_lm)) stop 413
    rank default
      stop 414
    end select
    call check_tk_ar(a, t, k, e, n)
    select rank(a)
    rank(1)
      if(any(a/=ref_c_ucs4_char_lm)) stop 415
    rank default
      stop 416
    end select
    return
  end subroutine c_check_c_ucs4_char_cm_ar

  subroutine f_check_c_ucs4_char_am_as(a)
    character(kind=c_ucs4_char, len=*), intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*m)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 417
    if(k/=4_c_signed_char) stop 418
    if(n/=m) stop 419
    if(int(k, kind=c_size_t)/=e) stop 420
    if(t/=CFI_type_ucs4_char) stop 421
    if(any(a/=ref_c_ucs4_char_lm)) stop 422
    call check_tk_as(a, t, k, e, n)
    if(any(a/=ref_c_ucs4_char_lm)) stop 423
    return
  end subroutine f_check_c_ucs4_char_am_as

  subroutine c_check_c_ucs4_char_am_as(a) bind(c)
    character(kind=c_ucs4_char, len=*), intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*m)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 424
    if(k/=4_c_signed_char) stop 425
    if(n/=m) stop 426
    if(int(k, kind=c_size_t)/=e) stop 427
    if(t/=CFI_type_ucs4_char) stop 428
    if(any(a/=ref_c_ucs4_char_lm)) stop 429
    call check_tk_as(a, t, k, e, n)
    if(any(a/=ref_c_ucs4_char_lm)) stop 430
    return
  end subroutine c_check_c_ucs4_char_am_as

  subroutine f_check_c_ucs4_char_am_ar(a)
    character(kind=c_ucs4_char, len=*), intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*m)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 431
    if(k/=4_c_signed_char) stop 432
    if(n/=m) stop 433
    if(int(k, kind=c_size_t)/=e) stop 434
    if(t/=CFI_type_ucs4_char) stop 435
    select rank(a)
    rank(1)
      if(any(a/=ref_c_ucs4_char_lm)) stop 436
    rank default
      stop 437
    end select
    call check_tk_ar(a, t, k, e, n)
    select rank(a)
    rank(1)
      if(any(a/=ref_c_ucs4_char_lm)) stop 438
    rank default
      stop 439
    end select
    return
  end subroutine f_check_c_ucs4_char_am_ar

  subroutine c_check_c_ucs4_char_am_ar(a) bind(c)
    character(kind=c_ucs4_char, len=*), intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*m)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 440
    if(k/=4_c_signed_char) stop 441
    if(n/=m) stop 442
    if(int(k, kind=c_size_t)/=e) stop 443
    if(t/=CFI_type_ucs4_char) stop 444
    select rank(a)
    rank(1)
      if(any(a/=ref_c_ucs4_char_lm)) stop 445
    rank default
      stop 446
    end select
    call check_tk_ar(a, t, k, e, n)
    select rank(a)
    rank(1)
      if(any(a/=ref_c_ucs4_char_lm)) stop 447
    rank default
      stop 448
    end select
    return
  end subroutine c_check_c_ucs4_char_am_ar

  subroutine f_check_c_ucs4_char_dm_as(a)
    character(kind=c_ucs4_char, len=:), pointer, intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*m)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 449
    if(k/=4_c_signed_char) stop 450
    if(n/=m) stop 451
    if(int(k, kind=c_size_t)/=e) stop 452
    if(t/=CFI_type_ucs4_char) stop 453
    if(any(a/=ref_c_ucs4_char_lm)) stop 454
    call check_tk_as(a, t, k, e, n)
    if(any(a/=ref_c_ucs4_char_lm)) stop 455
    return
  end subroutine f_check_c_ucs4_char_dm_as

  subroutine c_check_c_ucs4_char_dm_as(a) bind(c)
    character(kind=c_ucs4_char, len=:), pointer, intent(in) :: a(:)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*m)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 456
    if(k/=4_c_signed_char) stop 457
    if(n/=m) stop 458
    if(int(k, kind=c_size_t)/=e) stop 459
    if(t/=CFI_type_ucs4_char) stop 460
    if(any(a/=ref_c_ucs4_char_lm)) stop 461
    call check_tk_as(a, t, k, e, n)
    if(any(a/=ref_c_ucs4_char_lm)) stop 462
    return
  end subroutine c_check_c_ucs4_char_dm_as

  subroutine f_check_c_ucs4_char_dm_ar(a)
    character(kind=c_ucs4_char, len=:), pointer, intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*m)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 463
    if(k/=4_c_signed_char) stop 464
    if(n/=m) stop 465
    if(int(k, kind=c_size_t)/=e) stop 466
    if(t/=CFI_type_ucs4_char) stop 467
    select rank(a)
    rank(1)
      if(any(a/=ref_c_ucs4_char_lm)) stop 468
    rank default
      stop 469
    end select
    call check_tk_ar(a, t, k, e, n)
    select rank(a)
    rank(1)
      if(any(a/=ref_c_ucs4_char_lm)) stop 470
    rank default
      stop 471
    end select
    return
  end subroutine f_check_c_ucs4_char_dm_ar

  subroutine c_check_c_ucs4_char_dm_ar(a) bind(c)
    character(kind=c_ucs4_char, len=:), pointer, intent(in) :: a(..)
    !
    integer(kind=c_int16_t)     :: t
    integer(kind=c_signed_char) :: k
    integer(kind=c_size_t)      :: e, n
    !
    k = kind(a)
    n = len(a, kind=kind(e))
    e = storage_size(a, kind=kind(e))/(b*m)
    t = cfi_encode_type(CFI_type_Character, k)
    if(k<=0_c_signed_char) stop 472
    if(k/=4_c_signed_char) stop 473
    if(n/=m) stop 474
    if(int(k, kind=c_size_t)/=e) stop 475
    if(t/=CFI_type_ucs4_char) stop 476
    select rank(a)
    rank(1)
      if(any(a/=ref_c_ucs4_char_lm)) stop 477
    rank default
      stop 478
    end select
    call check_tk_ar(a, t, k, e, n)
    select rank(a)
    rank(1)
      if(any(a/=ref_c_ucs4_char_lm)) stop 479
    rank default
      stop 480
    end select
    return
  end subroutine c_check_c_ucs4_char_dm_ar
  
end module iso_check_m

program main_p
  
  use :: iso_check_m, only: &
    check_c_char_l1,        &
    check_c_char_lm,        &
    check_c_ucs4_char_l1,   &
    check_c_ucs4_char_lm

  implicit none

  call check_c_char_l1()
  call check_c_char_lm()
  ! See PR100907
  !call check_c_ucs4_char_l1()
  !call check_c_ucs4_char_lm()
  stop

end program main_p

!! Local Variables:
!! mode: f90
!! End:

