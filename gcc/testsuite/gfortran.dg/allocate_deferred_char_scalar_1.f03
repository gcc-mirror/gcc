! { dg-do run }
!
! Automatic reallocate on assignment, deferred length parameter for char
!
! PR fortran/45170
! PR fortran/35810
! PR fortran/47350
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
program test
  implicit none
  call mold_check()
  call mold_check4()
  call source_check()
  call source_check4()
  call ftn_test()
  call ftn_test4()
  call source3()
contains
  subroutine source_check()
    character(len=:), allocatable :: str, str2
    target :: str
    character(len=8) :: str3
    character(len=:), pointer :: str4, str5
    nullify(str4)
    str3 = 'AbCdEfGhIj'
    if(allocated(str)) STOP 1
    allocate(str, source=str3)
    if(.not.allocated(str)) STOP 2
    if(len(str) /= 8) STOP 3
    if(str /= 'AbCdEfGh') STOP 4
    if(associated(str4)) STOP 5
    str4 => str
    if(str4 /= str .or. len(str4)/=8) STOP 6
    if(.not.associated(str4, str)) STOP 7
    str4 => null()
    str = '12a56b78'
    if(str4 == '12a56b78') STOP 8
    str4 = 'ABCDEFGH'
    if(str == 'ABCDEFGH') STOP 9
    allocate(str5, source=str)
    if(associated(str5, str)) STOP 10
    if(str5 /= '12a56b78' .or. len(str5)/=8) STOP 11
    str = 'abcdef'
    if(str5 == 'abcdef') STOP 12
    str5 = 'ABCDEF'
    if(str == 'ABCDEF') STOP 13
  end subroutine source_check
  subroutine source_check4()
    character(kind=4,len=:), allocatable :: str, str2
    target :: str
    character(kind=4,len=8) :: str3
    character(kind=4,len=:), pointer :: str4, str5
    nullify(str4)
    str3 = 4_'AbCdEfGhIj'
    if(allocated(str)) STOP 14
    allocate(str, source=str3)
    if(.not.allocated(str)) STOP 15
    if(len(str) /= 8) STOP 16
    if(str /= 4_'AbCdEfGh') STOP 17
    if(associated(str4)) STOP 18
    str4 => str
    if(str4 /= str .or. len(str4)/=8) STOP 19
    if(.not.associated(str4, str)) STOP 20
    str4 => null()
    str = 4_'12a56b78'
    if(str4 == 4_'12a56b78') STOP 21
    str4 = 4_'ABCDEFGH'
    if(str == 4_'ABCDEFGH') STOP 22
    allocate(str5, source=str)
    if(associated(str5, str)) STOP 23
    if(str5 /= 4_'12a56b78' .or. len(str5)/=8) STOP 24
    str = 4_'abcdef'
    if(str5 == 4_'abcdef') STOP 25
    str5 = 4_'ABCDEF'
    if(str == 4_'ABCDEF') STOP 26
  end subroutine source_check4
  subroutine mold_check()
    character(len=:), allocatable :: str, str2
    character(len=8) :: str3
    character(len=:), pointer :: str4, str5
    nullify(str4)
    str2 = "ABCE"
    ALLOCATE( str, MOLD=str3)
    if (len(str) /= 8) STOP 27
    DEALLOCATE(str)
    ALLOCATE( str, MOLD=str2)
    if (len(str) /= 4) STOP 28

    IF (associated(str4)) STOP 29
    ALLOCATE( str4, MOLD=str3)
    IF (.not.associated(str4)) STOP 30
    str4 = '12345678'
    if (len(str4) /= 8) STOP 31
    if(str4 /= '12345678') STOP 32
    DEALLOCATE(str4)
    ALLOCATE( str4, MOLD=str2)
    str4 = 'ABCD'
    if (len(str4) /= 4) STOP 33
    if (str4 /= 'ABCD') STOP 34
    str5 => str4
    if(.not.associated(str4,str5)) STOP 35
    if(len(str5) /= 4 .or. len(str4) /= len(str5)) STOP 36
    if(str5 /= str4) STOP 37
    deallocate(str4) 
  end subroutine mold_check
  subroutine mold_check4()
    character(len=:,kind=4), allocatable :: str, str2
    character(len=8,kind=4) :: str3
    character(len=:,kind=4), pointer :: str4, str5
    nullify(str4)
    str2 = 4_"ABCE"
    ALLOCATE( str, MOLD=str3)
    if (len(str) /= 8) STOP 38
    DEALLOCATE(str)
    ALLOCATE( str, MOLD=str2)
    if (len(str) /= 4) STOP 39

    IF (associated(str4)) STOP 40
    ALLOCATE( str4, MOLD=str3)
    IF (.not.associated(str4)) STOP 41
    str4 = 4_'12345678'
    if (len(str4) /= 8) STOP 42
    if(str4 /= 4_'12345678') STOP 43
    DEALLOCATE(str4)
    ALLOCATE( str4, MOLD=str2)
    str4 = 4_'ABCD'
    if (len(str4) /= 4) STOP 44
    if (str4 /= 4_'ABCD') STOP 45
    str5 => str4
    if(.not.associated(str4,str5)) STOP 46
    if(len(str5) /= 4 .or. len(str4) /= len(str5)) STOP 47
    if(str5 /= str4) STOP 48
    deallocate(str4) 
  end subroutine mold_check4
  subroutine ftn_test()
    character(len=:), allocatable :: str_a
    character(len=:), pointer     :: str_p
    nullify(str_p) 
    call proc_test(str_a, str_p, .false.)
    if (str_p /= '123457890abcdef') STOP 49
    if (len(str_p) /= 50) STOP 50
    if (str_a(1:5) /= 'ABCDE ') STOP 51
    if (len(str_a) /= 50) STOP 52
    deallocate(str_p)
    str_a = '1245'
    if(len(str_a) /= 4) STOP 53
    if(str_a /= '1245') STOP 54
    allocate(character(len=6) :: str_p)
    if(len(str_p) /= 6) STOP 55
    str_p = 'AbCdEf'
    call proc_test(str_a, str_p, .true.)
    if (str_p /= '123457890abcdef') STOP 56
    if (len(str_p) /= 50) STOP 57
    if (str_a(1:5) /= 'ABCDE ') STOP 58
    if (len(str_a) /= 50) STOP 59
    deallocate(str_p)
  end subroutine ftn_test
  subroutine proc_test(a, p, alloc)
    character(len=:), allocatable :: a
    character(len=:), pointer     :: p
    character(len=5), target :: loc
    logical :: alloc
    if (.not.  alloc) then
      if(associated(p)) STOP 60
      if(allocated(a)) STOP 61
    else
      if(len(a) /= 4) STOP 62
      if(a /= '1245') STOP 63
      if(len(p) /= 6) STOP 64
      if(p /= 'AbCdEf') STOP 65
      deallocate(a)
      nullify(p)
    end if
    allocate(character(len=50) :: a)
    a(1:5) = 'ABCDE'
    if(len(a) /= 50) STOP 66
    if(a(1:5) /= "ABCDE") STOP 67
    loc = '12345'
    p => loc
    if (len(p) /= 5) STOP 68
    if (p /= '12345') STOP 69
    p = '12345679'
    if (len(p) /= 5) STOP 70
    if (p /= '12345') STOP 71
    p = 'ABC'
    if (loc /= 'ABC  ') STOP 72
    allocate(p, mold=a)
    if (.not.associated(p)) STOP 73
    p = '123457890abcdef'
    if (p /= '123457890abcdef') STOP 74
    if (len(p) /= 50) STOP 75
  end subroutine proc_test
  subroutine ftn_test4()
    character(len=:,kind=4), allocatable :: str_a
    character(len=:,kind=4), pointer     :: str_p
    nullify(str_p) 
    call proc_test4(str_a, str_p, .false.)
    if (str_p /= 4_'123457890abcdef') STOP 76
    if (len(str_p) /= 50) STOP 77
    if (str_a(1:5) /= 4_'ABCDE ') STOP 78
    if (len(str_a) /= 50) STOP 79
    deallocate(str_p)
    str_a = 4_'1245'
    if(len(str_a) /= 4) STOP 80
    if(str_a /= 4_'1245') STOP 81
    allocate(character(len=6, kind = 4) :: str_p)
    if(len(str_p) /= 6) STOP 82
    str_p = 4_'AbCdEf'
    call proc_test4(str_a, str_p, .true.)
    if (str_p /= 4_'123457890abcdef') STOP 83
    if (len(str_p) /= 50) STOP 84
    if (str_a(1:5) /= 4_'ABCDE ') STOP 85
    if (len(str_a) /= 50) STOP 86
    deallocate(str_p)
  end subroutine ftn_test4
  subroutine proc_test4(a, p, alloc)
    character(len=:,kind=4), allocatable :: a
    character(len=:,kind=4), pointer     :: p
    character(len=5,kind=4), target :: loc
    logical :: alloc
    if (.not.  alloc) then
      if(associated(p)) STOP 87
      if(allocated(a)) STOP 88
    else
      if(len(a) /= 4) STOP 89
      if(a /= 4_'1245') STOP 90
      if(len(p) /= 6) STOP 91
      if(p /= 4_'AbCdEf') STOP 92
      deallocate(a)
      nullify(p)
    end if
    allocate(character(len=50,kind=4) :: a)
    a(1:5) = 4_'ABCDE'
    if(len(a) /= 50) STOP 93
    if(a(1:5) /= 4_"ABCDE") STOP 94
    loc = '12345'
    p => loc
    if (len(p) /= 5) STOP 95
    if (p /= 4_'12345') STOP 96
    p = 4_'12345679'
    if (len(p) /= 5) STOP 97
    if (p /= 4_'12345') STOP 98
    p = 4_'ABC'
    if (loc /= 4_'ABC  ') STOP 99
    allocate(p, mold=a)
    if (.not.associated(p)) STOP 100
    p = 4_'123457890abcdef'
    if (p /= 4_'123457890abcdef') STOP 101
    if (len(p) /= 50) STOP 102
  end subroutine proc_test4
  subroutine source3()
     character(len=:, kind=1), allocatable :: a1
     character(len=:, kind=4), allocatable :: a4
     character(len=:, kind=1), pointer     :: p1
     character(len=:, kind=4), pointer     :: p4
     allocate(a1, source='ABC') ! << ICE
     if(len(a1) /= 3 .or. a1 /= 'ABC') STOP 103
     allocate(a4, source=4_'12345') ! << ICE
     if(len(a4) /= 5 .or. a4 /= 4_'12345') STOP 104
     allocate(p1, mold='AB') ! << ICE
     if(len(p1) /= 2) STOP 105
     allocate(p4, mold=4_'145') ! << ICE
     if(len(p4) /= 3) STOP 106
  end subroutine source3
end program test
! Spurious -Wstringop-overflow warning with -O1
! { dg-prune-output "\\\[-Wstringop-overflow=]" } 
