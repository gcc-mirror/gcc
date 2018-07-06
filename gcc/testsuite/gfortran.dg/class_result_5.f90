! { dg-do run }
!
! Test the fix for PR79072. The original problem was that an ICE
! would occur in the select type construct. On fixing that, it was
! found that the string length was not being transferred in the
! pointer assignment in the main program.
!
! Contributed by Neil Carlson  <neil.n.carlson@gmail.com>
!
function foo(string)
  class(*), pointer :: foo
  character(3), target :: string
  foo => string
  select type (foo)
    type is (character(*))
      if (foo .ne. 'foo') STOP 1
      foo = 'bar'
  end select
end function

  interface
    function foo(string)
      class(*), pointer :: foo
      character(3), target :: string
    end function
  end interface

  class(*), pointer :: res
  character(3), target :: string = 'foo'

  res => foo (string)

  select type (res)
    type is (character(*))
      if (res .ne. 'bar') STOP 2
  end select
  if (string .ne. 'bar') STOP 3
end
