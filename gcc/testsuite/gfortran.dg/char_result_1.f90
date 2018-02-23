! Related to PR 15326.  Try calling string functions whose lengths depend
! on the lengths of other strings.
! { dg-do run }
pure function double (string)
  character (len = *), intent (in) :: string
  character (len = len (string) * 2) :: double
  double = string // string
end function double

function f1 (string)
  character (len = *) :: string
  character (len = len (string)) :: f1
  f1 = ''
end function f1

function f2 (string1, string2)
  character (len = *) :: string1
  character (len = len (string1) - 20) :: string2
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
      character (len = *) :: string
      character (len = len (string)) :: f1
    end function f1
    function f2 (string1, string2)
      character (len = *) :: string1
      character (len = len (string1) - 20) :: string2
      character (len = len (string1) + len (string2) / 2) :: f2
    end function f2
  end interface

  integer :: a
  character (len = 80)  :: text
  character (len = 70), target :: textt
  character (len = 70), pointer :: textp

  a = 42
  textp => textt

  call test (f1 (text), 80)
  call test (f2 (text, text), 110)
  call test (f3 (text), 115)
  call test (f4 (text), 192)
  call test (f5 (text), 160)
  call test (f6 (text), 39)

  call test (f1 (textp), 70)
  call test (f2 (textp, text), 95)
  call test (f3 (textp), 105)
  call test (f4 (textp), 192)
  call test (f5 (textp), 140)
  call test (f6 (textp), 29)

  call indirect (textp)
contains
  function f3 (string)
    integer, parameter :: l1 = 30
    character (len = *) :: string
    character (len = len (string) + l1 + 5) :: f3
    f3 = ''
  end function f3

  function f4 (string)
    character (len = len (text) - 10) :: string
    character (len = len (string) + len (text) + a) :: f4
    f4 = ''
  end function f4

  function f5 (string)
    character (len = *) :: string
    character (len = len (double (string))) :: f5
    f5 = ''
  end function f5

  function f6 (string)
    character (len = *) :: string
    character (len = len (string (a:))) :: f6
    f6 = ''
  end function f6

  subroutine indirect (text2)
    character (len = *) :: text2

    call test (f1 (text), 80)
    call test (f2 (text, text), 110)
    call test (f3 (text), 115)
    call test (f4 (text), 192)
    call test (f5 (text), 160)
    call test (f6 (text), 39)

    call test (f1 (text2), 70)
    call test (f2 (text2, text2), 95)
    call test (f3 (text2), 105)
    call test (f4 (text2), 192)
    call test (f5 (text2), 140)
    call test (f6 (text2), 29)
  end subroutine indirect

  subroutine test (string, length)
    character (len = *) :: string
    integer, intent (in) :: length
    if (len (string) .ne. length) STOP 1
  end subroutine test
end program main
