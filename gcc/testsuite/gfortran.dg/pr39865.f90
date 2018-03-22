! PR fortran/39865
! { dg-do run }

subroutine f1 (a)
  character(len=1) :: a(7:)
  character(len=12) :: b
  character(len=1) :: c(2:10)
  write (b, a) 'Hell', 'o wo', 'rld!'
  if (b .ne. 'Hello world!') STOP 1
  write (b, a(:)) 'hell', 'o Wo', 'rld!'
  if (b .ne. 'hello World!') STOP 2
  write (b, a(8:)) 'Hell', 'o wo', 'rld!'
  if (b .ne. 'Hello world!') STOP 3
  c(2) = ' '
  c(3) = '('
  c(4) = '3'
  c(5) = 'A'
  c(6) = '4'
  c(7) = ')'
  write (b, c) 'hell', 'o Wo', 'rld!'
  if (b .ne. 'hello World!') STOP 4
  write (b, c(:)) 'Hell', 'o wo', 'rld!'
  if (b .ne. 'Hello world!') STOP 5
  write (b, c(3:)) 'hell', 'o Wo', 'rld!'
  if (b .ne. 'hello World!') STOP 6
end subroutine f1

subroutine f2 (a)
  character(len=1) :: a(10:,20:)
  character(len=12) :: b
  write (b, a) 'Hell', 'o wo', 'rld!'
  if (b .ne. 'Hello world!') STOP 7
  write (b, a) 'hell', 'o Wo', 'rld!'
  if (b .ne. 'hello World!') STOP 8
end subroutine f2

function f3 ()
  character(len=1) :: f3(5)
  f3(1) = '('
  f3(2) = '3'
  f3(3) = 'A'
  f3(4) = '4'
  f3(5) = ')'
end function f3

  interface
    subroutine f1 (a)
      character(len=1) :: a(:)
    end
  end interface
  interface
    subroutine f2 (a)
      character(len=1) :: a(:,:)
    end
  end interface
  interface
    function f3 ()
      character(len=1) :: f3(5)
    end
  end interface
  integer :: i, j
  character(len=1) :: e (6, 7:9), f (3,2), g (10)
  character(len=12) :: b
  e = 'X'
  e(2,8) = ' '
  e(3,8) = '('
  e(4,8) = '3'
  e(2,9) = 'A'
  e(3,9) = '4'
  e(4,9) = ')'
  f = e(2:4,8:9)
  g = 'X'
  g(2) = ' '
  g(3) = '('
  g(4) = '3'
  g(5) = 'A'
  g(6) = '4'
  g(7) = ')'
  call f1 (g(2:7))
  call f2 (f)
  call f2 (e(2:4,8:9))
  write (b, f3 ()) 'Hell', 'o wo', 'rld!'
  if (b .ne. 'Hello world!') STOP 9
end
