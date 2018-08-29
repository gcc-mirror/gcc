! Like char_result_1.f90, but the string arguments are pointers.
! { dg-do run }
pure function double (string)
  character (len = *), intent (in) :: string
  character (len = len (string) * 2) :: double
  double = string // string
end function double

function f1 (string)
  character (len = *), pointer :: string
  character (len = len (string)) :: f1
  f1 = ''
end function f1

function f2 (string1, string2)
  character (len = *), pointer :: string1
  character (len = len (string1) - 20), pointer :: string2
  character (len = len (string1) + len (string2) / 2) :: f2
  f2 = ''
end function f2

program main
  implicit none

  interface
    pure function double (string)
      character (len = *), intent (in) :: string
      character (len = len (string) * 2) :: double
    end function double
    function f1 (string)
      character (len = *), pointer :: string
      character (len = len (string)) :: f1
    end function f1
    function f2 (string1, string2)
      character (len = *), pointer :: string1
      character (len = len (string1) - 20), pointer :: string2
      character (len = len (string1) + len (string2) / 2) :: f2
    end function f2
  end interface

  integer :: a
  character (len = 80) :: text
  character (len = 70), target :: textt
  character (len = 70), pointer :: textp
  character (len = 50), pointer :: textp2

  a = 42
  textp => textt
  textp2 => textt(1:50)

  call test (f1 (textp), 70)
  call test (f2 (textp, textp), 95)
  call test (f3 (textp), 105)
  call test (f4 (textp), 192)
  call test (f5 (textp), 140)
  call test (f6 (textp), 29)

  call indirect (textp2)
contains
  function f3 (string)
    integer, parameter :: l1 = 30
    character (len = *), pointer :: string
    character (len = len (string) + l1 + 5) :: f3
    f3 = ''
  end function f3

  function f4 (string)
    character (len = len (text) - 10), pointer :: string
    character (len = len (string) + len (text) + a) :: f4
    f4 = ''
  end function f4

  function f5 (string)
    character (len = *), pointer :: string
    character (len = len (double (string))) :: f5
    f5 = ''
  end function f5

  function f6 (string)
    character (len = *), pointer :: string
    character (len = len (string (a:))) :: f6
    f6 = ''
  end function f6

  subroutine indirect (textp2)
    character (len = 50), pointer :: textp2

    call test (f1 (textp), 70)
    call test (f2 (textp, textp), 95)
    call test (f3 (textp), 105)
    call test (f4 (textp), 192)
    call test (f5 (textp), 140)
    call test (f6 (textp), 29)

    call test (f1 (textp2), 50)
    call test (f2 (textp2, textp), 65)
    call test (f3 (textp2), 85)
    call test (f5 (textp2), 100)
    call test (f6 (textp2), 9)
  end subroutine indirect

  subroutine test (string, length)
    character (len = *) :: string
    integer, intent (in) :: length
    if (len (string) .ne. length) STOP 1
  end subroutine test
end program main
