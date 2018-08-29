! { dg-do run }
! Test the fix for PR58085, where the offset for 'x' was set to zero,
! rather than -1.
!
! Contributed by Vladimir Fuka  <vladimir.fuka@gmail.com>
!
module foo
contains
  function bar (arg) result (res)
    integer arg, res(3)
    res = [arg, arg+1, arg +2]
  end function
end module
  use foo
  real d(3,3)
  integer a,b,c
  character(48) line1, line2
  associate (x=>shape(d))
    a = x(1)
    b = x(2)
    write (line1, *) a, b
    write (line2, *) x
    if (trim (line1) .ne. trim (line2)) STOP 1
  end associate
  associate (x=>[1,2])
    a = x(1)
    b = x(2)
    write (line1, *) a, b
    write (line2, *) x
    if (trim (line1) .ne. trim (line2)) STOP 2
  end associate
  associate (x=>bar(5)) ! make sure that we haven't broken function association
    a = x(1)
    b = x(2)
    c = x(3)
    write (line1, *) a, b, c
    write (line2, *) x
    if (trim (line1) .ne. trim (line2)) STOP 3
  end associate
end
