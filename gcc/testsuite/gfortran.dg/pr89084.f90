! PR fortran/89084
! { dg-do run }

integer function foo ()
  write (*,*) 'foo'
  block
    integer, parameter :: idxs(3) = (/ 1, 2, 3 /)
    integer :: i
    foo = 0
    do i = 1, size(idxs)
      foo = foo + idxs(i)
    enddo
  end block
end function foo
program pr89084
  integer :: i
  interface
    integer function foo ()
    end function
  end interface
  i = foo ()
  if (i.ne.6) stop 1
end
