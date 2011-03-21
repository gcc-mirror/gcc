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
    if(allocated(str)) call abort()
    allocate(str, source=str3)
    if(.not.allocated(str)) call abort()
    if(len(str) /= 8) call abort()
    if(str /= 'AbCdEfGh') call abort()
    if(associated(str4)) call abort()
    str4 => str
    if(str4 /= str .or. len(str4)/=8) call abort()
    if(.not.associated(str4, str)) call abort()
    str4 => null()
    str = '12a56b78'
    if(str4 == '12a56b78') call abort()
    str4 = 'ABCDEFGH'
    if(str == 'ABCDEFGH') call abort()
    allocate(str5, source=str)
    if(associated(str5, str)) call abort()
    if(str5 /= '12a56b78' .or. len(str5)/=8) call abort()
    str = 'abcdef'
    if(str5 == 'abcdef') call abort()
    str5 = 'ABCDEF'
    if(str == 'ABCDEF') call abort()
  end subroutine source_check
  subroutine source_check4()
    character(kind=4,len=:), allocatable :: str, str2
    target :: str
    character(kind=4,len=8) :: str3
    character(kind=4,len=:), pointer :: str4, str5
    nullify(str4)
    str3 = 4_'AbCdEfGhIj'
    if(allocated(str)) call abort()
    allocate(str, source=str3)
    if(.not.allocated(str)) call abort()
    if(len(str) /= 8) call abort()
    if(str /= 4_'AbCdEfGh') call abort()
    if(associated(str4)) call abort()
    str4 => str
    if(str4 /= str .or. len(str4)/=8) call abort()
    if(.not.associated(str4, str)) call abort()
    str4 => null()
    str = 4_'12a56b78'
    if(str4 == 4_'12a56b78') call abort()
    str4 = 4_'ABCDEFGH'
    if(str == 4_'ABCDEFGH') call abort()
    allocate(str5, source=str)
    if(associated(str5, str)) call abort()
    if(str5 /= 4_'12a56b78' .or. len(str5)/=8) call abort()
    str = 4_'abcdef'
    if(str5 == 4_'abcdef') call abort()
    str5 = 4_'ABCDEF'
    if(str == 4_'ABCDEF') call abort()
  end subroutine source_check4
  subroutine mold_check()
    character(len=:), allocatable :: str, str2
    character(len=8) :: str3
    character(len=:), pointer :: str4, str5
    nullify(str4)
    str2 = "ABCE"
    ALLOCATE( str, MOLD=str3)
    if (len(str) /= 8) call abort()
    DEALLOCATE(str)
    ALLOCATE( str, MOLD=str2)
    if (len(str) /= 4) call abort()

    IF (associated(str4)) call abort()
    ALLOCATE( str4, MOLD=str3)
    IF (.not.associated(str4)) call abort()
    str4 = '12345678'
    if (len(str4) /= 8) call abort()
    if(str4 /= '12345678') call abort()
    DEALLOCATE(str4)
    ALLOCATE( str4, MOLD=str2)
    str4 = 'ABCD'
    if (len(str4) /= 4) call abort()
    if (str4 /= 'ABCD') call abort()
    str5 => str4
    if(.not.associated(str4,str5)) call abort()
    if(len(str5) /= 4 .or. len(str4) /= len(str5)) call abort()
    if(str5 /= str4) call abort()
    deallocate(str4) 
  end subroutine mold_check
  subroutine mold_check4()
    character(len=:,kind=4), allocatable :: str, str2
    character(len=8,kind=4) :: str3
    character(len=:,kind=4), pointer :: str4, str5
    nullify(str4)
    str2 = 4_"ABCE"
    ALLOCATE( str, MOLD=str3)
    if (len(str) /= 8) call abort()
    DEALLOCATE(str)
    ALLOCATE( str, MOLD=str2)
    if (len(str) /= 4) call abort()

    IF (associated(str4)) call abort()
    ALLOCATE( str4, MOLD=str3)
    IF (.not.associated(str4)) call abort()
    str4 = 4_'12345678'
    if (len(str4) /= 8) call abort()
    if(str4 /= 4_'12345678') call abort()
    DEALLOCATE(str4)
    ALLOCATE( str4, MOLD=str2)
    str4 = 4_'ABCD'
    if (len(str4) /= 4) call abort()
    if (str4 /= 4_'ABCD') call abort()
    str5 => str4
    if(.not.associated(str4,str5)) call abort()
    if(len(str5) /= 4 .or. len(str4) /= len(str5)) call abort()
    if(str5 /= str4) call abort()
    deallocate(str4) 
  end subroutine mold_check4
  subroutine ftn_test()
    character(len=:), allocatable :: str_a
    character(len=:), pointer     :: str_p
    nullify(str_p) 
    call proc_test(str_a, str_p, .false.)
    if (str_p /= '123457890abcdef') call abort()
    if (len(str_p) /= 50) call abort()
    if (str_a(1:5) /= 'ABCDE ') call abort()
    if (len(str_a) /= 50) call abort()
    deallocate(str_p)
    str_a = '1245'
    if(len(str_a) /= 4) call abort()
    if(str_a /= '1245') call abort()
    allocate(character(len=6) :: str_p)
    if(len(str_p) /= 6) call abort()
    str_p = 'AbCdEf'
    call proc_test(str_a, str_p, .true.)
    if (str_p /= '123457890abcdef') call abort()
    if (len(str_p) /= 50) call abort()
    if (str_a(1:5) /= 'ABCDE ') call abort()
    if (len(str_a) /= 50) call abort()
    deallocate(str_p)
  end subroutine ftn_test
  subroutine proc_test(a, p, alloc)
    character(len=:), allocatable :: a
    character(len=:), pointer     :: p
    character(len=5), target :: loc
    logical :: alloc
    if (.not.  alloc) then
      if(associated(p)) call abort()
      if(allocated(a)) call abort()
    else
      if(len(a) /= 4) call abort()
      if(a /= '1245') call abort()
      if(len(p) /= 6) call abort()
      if(p /= 'AbCdEf') call abort()
      deallocate(a)
      nullify(p)
    end if
    allocate(character(len=50) :: a)
    a(1:5) = 'ABCDE'
    if(len(a) /= 50) call abort()
    if(a(1:5) /= "ABCDE") call abort()
    loc = '12345'
    p => loc
    if (len(p) /= 5) call abort()
    if (p /= '12345') call abort()
    p = '12345679'
    if (len(p) /= 5) call abort()
    if (p /= '12345') call abort()
    p = 'ABC'
    if (loc /= 'ABC  ') call abort()
    allocate(p, mold=a)
    if (.not.associated(p)) call abort()
    p = '123457890abcdef'
    if (p /= '123457890abcdef') call abort()
    if (len(p) /= 50) call abort()
  end subroutine proc_test
  subroutine ftn_test4()
    character(len=:,kind=4), allocatable :: str_a
    character(len=:,kind=4), pointer     :: str_p
    nullify(str_p) 
    call proc_test4(str_a, str_p, .false.)
    if (str_p /= 4_'123457890abcdef') call abort()
    if (len(str_p) /= 50) call abort()
    if (str_a(1:5) /= 4_'ABCDE ') call abort()
    if (len(str_a) /= 50) call abort()
    deallocate(str_p)
    str_a = 4_'1245'
    if(len(str_a) /= 4) call abort()
    if(str_a /= 4_'1245') call abort()
    allocate(character(len=6, kind = 4) :: str_p)
    if(len(str_p) /= 6) call abort()
    str_p = 4_'AbCdEf'
    call proc_test4(str_a, str_p, .true.)
    if (str_p /= 4_'123457890abcdef') call abort()
    if (len(str_p) /= 50) call abort()
    if (str_a(1:5) /= 4_'ABCDE ') call abort()
    if (len(str_a) /= 50) call abort()
    deallocate(str_p)
  end subroutine ftn_test4
  subroutine proc_test4(a, p, alloc)
    character(len=:,kind=4), allocatable :: a
    character(len=:,kind=4), pointer     :: p
    character(len=5,kind=4), target :: loc
    logical :: alloc
    if (.not.  alloc) then
      if(associated(p)) call abort()
      if(allocated(a)) call abort()
    else
      if(len(a) /= 4) call abort()
      if(a /= 4_'1245') call abort()
      if(len(p) /= 6) call abort()
      if(p /= 4_'AbCdEf') call abort()
      deallocate(a)
      nullify(p)
    end if
    allocate(character(len=50,kind=4) :: a)
    a(1:5) = 4_'ABCDE'
    if(len(a) /= 50) call abort()
    if(a(1:5) /= 4_"ABCDE") call abort()
    loc = '12345'
    p => loc
    if (len(p) /= 5) call abort()
    if (p /= 4_'12345') call abort()
    p = 4_'12345679'
    if (len(p) /= 5) call abort()
    if (p /= 4_'12345') call abort()
    p = 4_'ABC'
    if (loc /= 4_'ABC  ') call abort()
    allocate(p, mold=a)
    if (.not.associated(p)) call abort()
    p = 4_'123457890abcdef'
    if (p /= 4_'123457890abcdef') call abort()
    if (len(p) /= 50) call abort()
  end subroutine proc_test4
  subroutine source3()
     character(len=:, kind=1), allocatable :: a1
     character(len=:, kind=4), allocatable :: a4
     character(len=:, kind=1), pointer     :: p1
     character(len=:, kind=4), pointer     :: p4
     allocate(a1, source='ABC') ! << ICE
     if(len(a1) /= 3 .or. a1 /= 'ABC') call abort()
     allocate(a4, source=4_'12345') ! << ICE
     if(len(a4) /= 5 .or. a4 /= 4_'12345') call abort()
     allocate(p1, mold='AB') ! << ICE
     if(len(p1) /= 2) call abort()
     allocate(p4, mold=4_'145') ! << ICE
     if(len(p4) /= 3) call abort()
  end subroutine source3
end program test
