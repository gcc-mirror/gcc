! { dg-do run }
! Testing fix for PR fortran/60289
! Contributed by: Andre Vehreschild <vehre@gmx.de>
!
program test
    implicit none

    class(*), pointer :: P1, P2, P3
    class(*), pointer, dimension(:) :: PA1
    class(*), allocatable :: A1, A2
    integer :: string_len = 10 *2
    character(len=:), allocatable, target :: str
    character(len=:,kind=4), allocatable :: str4
    type T
        class(*), pointer :: content
    end type
    type(T) :: o1, o2

    str = "string for test"
    str4 = 4_"string for test"

    allocate(character(string_len)::P1)

    select type(P1)
        type is (character(*))
            P1 ="some test string"
            if (P1 .ne. "some test string") STOP 1
            if (len(P1) .ne. 20) STOP 2
            if (len(P1) .eq. len("some test string")) STOP 3
        class default
            STOP 4
    end select

    allocate(A1, source = P1)

    select type(A1)
        type is (character(*))
            if (A1 .ne. "some test string") STOP 5
            if (len(A1) .ne. 20) STOP 6
            if (len(A1) .eq. len("some test string")) STOP 7
        class default
            STOP 8
    end select

    allocate(A2, source = convertType(P1))

    select type(A2)
        type is (character(*))
            if (A2 .ne. "some test string") STOP 9
            if (len(A2) .ne. 20) STOP 10
            if (len(A2) .eq. len("some test string")) STOP 11
        class default
            STOP 12
    end select

    allocate(P2, source = str)

    select type(P2)
        type is (character(*))
            if (P2 .ne. "string for test") STOP 13
            if (len(P2) .eq. 20) STOP 14
            if (len(P2) .ne. len("string for test")) STOP 15
        class default
            STOP 16
    end select

    allocate(P3, source = "string for test")

    select type(P3)
        type is (character(*))
            if (P3 .ne. "string for test") STOP 17
            if (len(P3) .eq. 20) STOP 18
            if (len(P3) .ne. len("string for test")) STOP 19
        class default
            STOP 20
    end select

    allocate(character(len=10)::PA1(3))

    select type(PA1)
        type is (character(*))
            PA1(1) = "string 10 "
            if (PA1(1) .ne. "string 10 ") STOP 21
            if (any(len(PA1(:)) .ne. [10,10,10])) STOP 22
        class default
            STOP 23
    end select

    deallocate(PA1)
    deallocate(P3)
!   if (len(P3) .ne. 0) STOP 24 ! Can't check, because select
!     type would be needed, which needs the vptr, which is 0 now.
    deallocate(P2)
    deallocate(A2)
    deallocate(A1)
    deallocate(P1)

    ! Now for kind=4 chars.

    allocate(character(len=20,kind=4)::P1)

    select type(P1)
        type is (character(len=*,kind=4))
            P1 ="some test string"
            if (P1 .ne. 4_"some test string") STOP 25
            if (len(P1) .ne. 20) STOP 26
            if (len(P1) .eq. len("some test string")) STOP 27
        type is (character(len=*,kind=1))
            STOP 28
        class default
            STOP 29
    end select

    allocate(A1, source=P1)

    select type(A1)
        type is (character(len=*,kind=4))
            if (A1 .ne. 4_"some test string") STOP 30
            if (len(A1) .ne. 20) STOP 31
            if (len(A1) .eq. len("some test string")) STOP 32
        type is (character(len=*,kind=1))
            STOP 33
        class default
            STOP 34
    end select

    allocate(A2, source = convertType(P1))

    select type(A2)
        type is (character(len=*, kind=4))
            if (A2 .ne. 4_"some test string") STOP 35
            if (len(A2) .ne. 20) STOP 36
            if (len(A2) .eq. len("some test string")) STOP 37
        class default
            STOP 38
    end select

    allocate(P2, source = str4)

    select type(P2)
        type is (character(len=*,kind=4))
            if (P2 .ne. 4_"string for test") STOP 39
            if (len(P2) .eq. 20) STOP 40
            if (len(P2) .ne. len("string for test")) STOP 41
        class default
            STOP 42
    end select

    allocate(P3, source = convertType(P2))

    select type(P3)
        type is (character(len=*, kind=4))
            if (P3 .ne. 4_"string for test") STOP 43
            if (len(P3) .eq. 20) STOP 44
            if (len(P3) .ne. len("string for test")) STOP 45
        class default
            STOP 46
    end select

    allocate(character(kind=4, len=10)::PA1(3))

    select type(PA1)
        type is (character(len=*, kind=4))
            PA1(1) = 4_"string 10 "
            if (PA1(1) .ne. 4_"string 10 ") STOP 47
            if (any(len(PA1(:)) .ne. [10,10,10])) STOP 48
        class default
            STOP 49
    end select

    deallocate(PA1)
    deallocate(P3)
    deallocate(P2)
    deallocate(A2)
    deallocate(P1)
    deallocate(A1)

    allocate(o1%content, source='test string')
    allocate(o2%content, source=o1%content)
    select type (c => o1%content)
      type is (character(*))
        if (c /= 'test string') STOP 50
      class default
        STOP 51
    end select
    select type (d => o2%content)
      type is (character(*))
        if (d /= 'test string') STOP 52
      class default
    end select

    call AddCopy ('test string')

contains

  function convertType(in)
    class(*), pointer, intent(in) :: in
    class(*), pointer :: convertType

    convertType => in
  end function

  subroutine AddCopy(C)
    class(*), intent(in) :: C
    class(*), pointer :: P
    allocate(P, source=C)
    select type (P)
      type is (character(*))
        if (P /= 'test string') STOP 53
      class default
        STOP 54
    end select
  end subroutine

end program test
