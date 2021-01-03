! PR fortran/97768
! { dg-do compile }

module pr97768_2
  interface operator(.in.)
    module procedure substr_in_str
  end interface
contains
  pure function to_upper (in_str) result (string)
    character(len=*), intent(in) :: in_str
    character(len=len(in_str)) :: string
    string = in_str
  end function to_upper
  logical pure function substr_in_str (substring, string)
    character(len=*), intent(in) :: string, substring
    substr_in_str=.false.
  end function
end module
function foo ()
  use pr97768_2, only : to_upper, operator(.in.)
  logical :: foo
  character(len=8) :: str
  str = 'abcde'
  foo = to_upper (str) .in. 32    ! { dg-error "are CHARACTER/INTEGER" }
end function foo
function bar (str)
  use pr97768_2, only : operator(.in.)
  logical :: bar
  character(len=*) :: str
  foo = str .in. 32               ! { dg-error "are CHARACTER\\(\\*\\)/INTEGER" }
end function bar
function baz (lenstr)
  use pr97768_2, only : operator(.in.)
  logical :: baz
  integer :: lenstr
  character(len=lenstr) :: str
  str = 'abc'
  foo = str .in. 32               ! { dg-error "are CHARACTER/INTEGER" }
end function baz
function qux ()
  use pr97768_2, only : operator(.in.)
  logical :: qux
  character(len=8) :: str
  str = 'def'
  foo = str .in. 32               ! { dg-error "are CHARACTER\\(8\\)/INTEGER" }
end function qux
function corge ()
  use pr97768_2, only : operator(.in.)
  logical :: corge
  character(len=:), allocatable :: str
  str = 'ghijk'
  foo = str .in. 32               ! { dg-error "are CHARACTER\\(:\\)/INTEGER" }
end function corge
