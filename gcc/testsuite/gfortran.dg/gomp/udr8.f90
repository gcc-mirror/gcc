! { dg-do compile }
! { dg-options "-fmax-errors=1000 -fopenmp" }

module m
contains
  function fn1 (x, y)
    integer, intent(in) :: x, y
    integer :: fn1
    fn1 = x + 2 * y
  end function
  subroutine sub1 (x, y)
    integer, intent(in) :: y
    integer, intent(out) :: x
    x = y
  end subroutine
  function fn2 (x)
    integer, intent(in) :: x
    integer :: fn2
    fn2 = x
  end function
  subroutine sub2 (x, y)
    integer, intent(in) :: y
    integer, intent(inout) :: x
    x = x + y
  end subroutine
  function fn3 (x, y)
    integer, intent(in) :: x(:), y(:)
    integer :: fn3(lbound(x, 1):ubound(x, 1))
    fn3 = x + 2 * y
  end function
  subroutine sub3 (x, y)
    integer, intent(in) :: y(:)
    integer, intent(out) :: x(:)
    x = y
  end subroutine
  function fn4 (x)
    integer, intent(in) :: x(:)
    integer :: fn4(lbound(x, 1):ubound(x, 1))
    fn4 = x
  end function
  subroutine sub4 (x, y)
    integer, intent(in) :: y(:)
    integer, intent(inout) :: x(:)
    x = x + y
  end subroutine
  function fn5 (x, y)
    integer, intent(in) :: x(10), y(10)
    integer :: fn5(10)
    fn5 = x + 2 * y
  end function
  subroutine sub5 (x, y)
    integer, intent(in) :: y(10)
    integer, intent(out) :: x(10)
    x = y
  end subroutine
  function fn6 (x)
    integer, intent(in) :: x(10)
    integer :: fn6(10)
    fn6 = x
  end function
  subroutine sub6 (x, y)
    integer, intent(in) :: y(10)
    integer, intent(inout) :: x(10)
    x = x + y
  end subroutine
  function fn7 (x, y)
    integer, allocatable, intent(in) :: x(:), y(:)
    integer, allocatable :: fn7(:)
    fn7 = x + 2 * y
  end function
  subroutine sub7 (x, y)
    integer, allocatable, intent(in) :: y(:)
    integer, allocatable, intent(out) :: x(:)
    x = y
  end subroutine
  function fn8 (x)
    integer, allocatable, intent(in) :: x(:)
    integer, allocatable :: fn8(:)
    fn8 = x
  end function
  subroutine sub8 (x, y)
    integer, allocatable, intent(in) :: y(:)
    integer, allocatable, intent(inout) :: x(:)
    x = x + y
  end subroutine
end module
subroutine test1
  use m
!$omp declare reduction (foo : integer : omp_out = omp_out + omp_in) initializer (omp_priv = 0)
!$omp declare reduction (bar : integer : omp_out = fn1 (omp_out, omp_in)) & ! { dg-error "Rank mismatch in argument\[^\n\r]*scalar and rank-1" }
!$omp & initializer (sub1 (omp_priv, omp_orig)) ! { dg-error "Rank mismatch in argument\[^\n\r]*scalar and rank-1" }
!$omp declare reduction (baz : integer : sub2 (omp_out, omp_in)) & ! { dg-error "Rank mismatch in argument\[^\n\r]*scalar and rank-1" }
!$omp initializer (omp_priv = fn2 (omp_orig)) ! { dg-error "Rank mismatch in argument\[^\n\r]*scalar and rank-1" }
  integer :: a(10)
!$omp parallel reduction (foo : a)
!$omp end parallel
!$omp parallel reduction (bar : a)
!$omp end parallel
!$omp parallel reduction (baz : a)
!$omp end parallel
end subroutine test1
subroutine test2
  use m
!$omp declare reduction (foo : integer : omp_out = omp_out + omp_in) initializer (omp_priv = 0)
!$omp declare reduction (bar : integer : omp_out = fn1 (omp_out, omp_in)) &
!$omp & initializer (sub1 (omp_priv, omp_orig))
!$omp declare reduction (baz : integer : sub2 (omp_out, omp_in)) &
!$omp initializer (omp_priv = fn2 (omp_orig))
  integer :: a
!$omp parallel reduction (foo : a)
!$omp end parallel
!$omp parallel reduction (bar : a)
!$omp end parallel
!$omp parallel reduction (baz : a)
!$omp end parallel
end subroutine test2
subroutine test3
  use m
!$omp declare reduction (foo : integer : omp_out = omp_out + omp_in) initializer (omp_priv = 0)
!$omp declare reduction (bar : integer : omp_out = fn1 (omp_out, omp_in)) & ! { dg-error "Rank mismatch in argument\[^\n\r]*scalar and rank-1" }
!$omp & initializer (sub1 (omp_priv, omp_orig)) ! { dg-error "Rank mismatch in argument\[^\n\r]*scalar and rank-1" }
!$omp declare reduction (baz : integer : sub2 (omp_out, omp_in)) & ! { dg-error "Rank mismatch in argument\[^\n\r]*scalar and rank-1" }
!$omp initializer (omp_priv = fn2 (omp_orig)) ! { dg-error "Rank mismatch in argument\[^\n\r]*scalar and rank-1" }
  integer, allocatable :: a(:)
  allocate (a(10))
!$omp parallel reduction (foo : a)
!$omp end parallel
!$omp parallel reduction (bar : a)
!$omp end parallel
!$omp parallel reduction (baz : a)
!$omp end parallel
end subroutine test3
subroutine test4
  use m
!$omp declare reduction (foo : integer : omp_out = omp_out + omp_in) initializer (omp_priv = 0)
!$omp declare reduction (bar : integer : omp_out = fn1 (omp_out, omp_in)) &
!$omp & initializer (sub1 (omp_priv, omp_orig))
!$omp declare reduction (baz : integer : sub2 (omp_out, omp_in)) &
!$omp initializer (omp_priv = fn2 (omp_orig))
  integer, allocatable :: a
  allocate (a)
!$omp parallel reduction (foo : a)
!$omp end parallel
!$omp parallel reduction (bar : a)
!$omp end parallel
!$omp parallel reduction (baz : a)
!$omp end parallel
end subroutine test4
subroutine test5
  use m
!$omp declare reduction (foo : integer : omp_out = omp_out + omp_in) initializer (omp_priv = 0)
!$omp declare reduction (bar : integer : omp_out = fn3 (omp_out, omp_in)) &
!$omp & initializer (sub3 (omp_priv, omp_orig))
!$omp declare reduction (baz : integer : sub4 (omp_out, omp_in)) &
!$omp initializer (omp_priv = fn4 (omp_orig))
  integer :: a(10)
!$omp parallel reduction (foo : a)
!$omp end parallel
!$omp parallel reduction (bar : a)
!$omp end parallel
!$omp parallel reduction (baz : a)
!$omp end parallel
end subroutine test5
subroutine test6
  use m
!$omp declare reduction (foo : integer : omp_out = omp_out + omp_in) initializer (omp_priv = 0)
!$omp declare reduction (bar : integer : omp_out = fn3 (omp_out, omp_in)) & ! { dg-error "Rank mismatch in argument\[^\n\r]*rank-1 and scalar|Incompatible ranks 0 and 1 in assignment" }
!$omp & initializer (sub3 (omp_priv, omp_orig)) ! { dg-error "Rank mismatch in argument\[^\n\r]*rank-1 and scalar" }
!$omp declare reduction (baz : integer : sub4 (omp_out, omp_in)) & ! { dg-error "Rank mismatch in argument\[^\n\r]*rank-1 and scalar" }
!$omp initializer (omp_priv = fn4 (omp_orig)) ! { dg-error "Rank mismatch in argument\[^\n\r]*rank-1 and scalar|Incompatible ranks 0 and 1 in assignment" }
  integer :: a
!$omp parallel reduction (foo : a)
!$omp end parallel
!$omp parallel reduction (bar : a)
!$omp end parallel
!$omp parallel reduction (baz : a)
!$omp end parallel
end subroutine test6
subroutine test7
  use m
!$omp declare reduction (foo : integer : omp_out = omp_out + omp_in) initializer (omp_priv = 0)
!$omp declare reduction (bar : integer : omp_out = fn3 (omp_out, omp_in)) &
!$omp & initializer (sub3 (omp_priv, omp_orig))
!$omp declare reduction (baz : integer : sub4 (omp_out, omp_in)) &
!$omp initializer (omp_priv = fn4 (omp_orig))
  integer, allocatable :: a(:)
  allocate (a(10))
!$omp parallel reduction (foo : a)
!$omp end parallel
!$omp parallel reduction (bar : a)
!$omp end parallel
!$omp parallel reduction (baz : a)
!$omp end parallel
end subroutine test7
subroutine test8
  use m
!$omp declare reduction (foo : integer : omp_out = omp_out + omp_in) initializer (omp_priv = 0)
!$omp declare reduction (bar : integer : omp_out = fn3 (omp_out, omp_in)) & ! { dg-error "Rank mismatch in argument\[^\n\r]*rank-1 and scalar|Incompatible ranks 0 and 1 in assignment" }
!$omp & initializer (sub3 (omp_priv, omp_orig)) ! { dg-error "Rank mismatch in argument\[^\n\r]*rank-1 and scalar" }
!$omp declare reduction (baz : integer : sub4 (omp_out, omp_in)) & ! { dg-error "Rank mismatch in argument\[^\n\r]*rank-1 and scalar" }
!$omp initializer (omp_priv = fn4 (omp_orig)) ! { dg-error "Rank mismatch in argument\[^\n\r]*rank-1 and scalar|Incompatible ranks 0 and 1 in assignment" }
  integer, allocatable :: a
  allocate (a)
!$omp parallel reduction (foo : a)
!$omp end parallel
!$omp parallel reduction (bar : a)
!$omp end parallel
!$omp parallel reduction (baz : a)
!$omp end parallel
end subroutine test8
subroutine test9
  use m
!$omp declare reduction (foo : integer : omp_out = omp_out + omp_in) initializer (omp_priv = 0)
!$omp declare reduction (bar : integer : omp_out = fn5 (omp_out, omp_in)) &
!$omp & initializer (sub5 (omp_priv, omp_orig))
!$omp declare reduction (baz : integer : sub6 (omp_out, omp_in)) &
!$omp initializer (omp_priv = fn6 (omp_orig))
  integer :: a(10)
!$omp parallel reduction (foo : a)
!$omp end parallel
!$omp parallel reduction (bar : a)
!$omp end parallel
!$omp parallel reduction (baz : a)
!$omp end parallel
end subroutine test9
subroutine test10
  use m
!$omp declare reduction (foo : integer : omp_out = omp_out + omp_in) initializer (omp_priv = 0)
!$omp declare reduction (bar : integer : omp_out = fn5 (omp_out, omp_in)) & ! { dg-error "Rank mismatch in argument\[^\n\r]*rank-1 and scalar|Incompatible ranks 0 and 1 in assignment" }
!$omp & initializer (sub5 (omp_priv, omp_orig)) ! { dg-error "Rank mismatch in argument\[^\n\r]*rank-1 and scalar" }
!$omp declare reduction (baz : integer : sub6 (omp_out, omp_in)) & ! { dg-error "Rank mismatch in argument\[^\n\r]*rank-1 and scalar" }
!$omp initializer (omp_priv = fn6 (omp_orig)) ! { dg-error "Rank mismatch in argument\[^\n\r]*rank-1 and scalar|Incompatible ranks 0 and 1 in assignment" }
  integer :: a
!$omp parallel reduction (foo : a)
!$omp end parallel
!$omp parallel reduction (bar : a)
!$omp end parallel
!$omp parallel reduction (baz : a)
!$omp end parallel
end subroutine test10
subroutine test11
  use m
!$omp declare reduction (foo : integer : omp_out = omp_out + omp_in) initializer (omp_priv = 0)
!$omp declare reduction (bar : integer : omp_out = fn5 (omp_out, omp_in)) &
!$omp & initializer (sub5 (omp_priv, omp_orig))
!$omp declare reduction (baz : integer : sub6 (omp_out, omp_in)) &
!$omp initializer (omp_priv = fn6 (omp_orig))
  integer, allocatable :: a(:)
  allocate (a(10))
!$omp parallel reduction (foo : a)
!$omp end parallel
!$omp parallel reduction (bar : a)
!$omp end parallel
!$omp parallel reduction (baz : a)
!$omp end parallel
end subroutine test11
subroutine test12
  use m
!$omp declare reduction (foo : integer : omp_out = omp_out + omp_in) initializer (omp_priv = 0)
!$omp declare reduction (bar : integer : omp_out = fn5 (omp_out, omp_in)) & ! { dg-error "Rank mismatch in argument\[^\n\r]*rank-1 and scalar|Incompatible ranks 0 and 1 in assignment" }
!$omp & initializer (sub5 (omp_priv, omp_orig)) ! { dg-error "Rank mismatch in argument\[^\n\r]*rank-1 and scalar" }
!$omp declare reduction (baz : integer : sub6 (omp_out, omp_in)) & ! { dg-error "Rank mismatch in argument\[^\n\r]*rank-1 and scalar" }
!$omp initializer (omp_priv = fn6 (omp_orig)) ! { dg-error "Rank mismatch in argument\[^\n\r]*rank-1 and scalar|Incompatible ranks 0 and 1 in assignment" }
  integer, allocatable :: a
  allocate (a)
!$omp parallel reduction (foo : a)
!$omp end parallel
!$omp parallel reduction (bar : a)
!$omp end parallel
!$omp parallel reduction (baz : a)
!$omp end parallel
end subroutine test12
subroutine test13
  use m
!$omp declare reduction (foo : integer : omp_out = omp_out + omp_in) initializer (omp_priv = 0)
!$omp declare reduction (bar : integer : omp_out = & ! { dg-error "Different shape for array assignment at \[^\n\r]* on dimension 1 .9 and 10" }
!$omp & fn5 (omp_out, omp_in)) & ! { dg-warning "Actual argument contains too few elements for dummy argument \[^\n\r]* .9/10" }
!$omp & initializer (sub5 (omp_priv, omp_orig)) ! { dg-warning "Actual argument contains too few elements for dummy argument \[^\n\r]* .9/10" }
!$omp declare reduction (baz : integer : sub6 (omp_out, omp_in)) & ! { dg-warning "Actual argument contains too few elements for dummy argument \[^\n\r]* .9/10" }
!$omp initializer (omp_priv = & ! { dg-error "Different shape for array assignment at \[^\n\r]* on dimension 1 .9 and 10" }
!$omp & fn6 (omp_orig)) ! { dg-warning "Actual argument contains too few elements for dummy argument \[^\n\r]* .9/10" }
  integer :: a(9)
!$omp parallel reduction (foo : a)
!$omp end parallel
!$omp parallel reduction (bar : a)
!$omp end parallel
!$omp parallel reduction (baz : a)
!$omp end parallel
end subroutine test13
subroutine test14
  use m
!$omp declare reduction (foo : integer : omp_out = omp_out + omp_in) initializer (omp_priv = 0)
!$omp declare reduction (bar : integer : omp_out = fn7 (omp_out, omp_in)) & ! { dg-error "Actual argument for \[^\n\r]* must be ALLOCATABLE" }
!$omp & initializer (sub7 (omp_priv, omp_orig)) ! { dg-error "Actual argument for \[^\n\r]* must be ALLOCATABLE" }
!$omp declare reduction (baz : integer : sub8 (omp_out, omp_in)) & ! { dg-error "Actual argument for \[^\n\r]* must be ALLOCATABLE" }
!$omp initializer (omp_priv = fn8 (omp_orig)) ! { dg-error "Actual argument for \[^\n\r]* must be ALLOCATABLE" }
  integer :: a(10)
!$omp parallel reduction (foo : a)
!$omp end parallel
!$omp parallel reduction (bar : a)
!$omp end parallel
!$omp parallel reduction (baz : a)
!$omp end parallel
end subroutine test14
subroutine test15
  use m
!$omp declare reduction (foo : integer : omp_out = omp_out + omp_in) initializer (omp_priv = 0)
!$omp declare reduction (bar : integer : omp_out = fn7 (omp_out, omp_in)) & ! { dg-error "Rank mismatch in argument\[^\n\r]*rank-1 and scalar|Incompatible ranks 0 and 1 in assignment" }
!$omp & initializer (sub7 (omp_priv, omp_orig)) ! { dg-error "Rank mismatch in argument\[^\n\r]*rank-1 and scalar" }
!$omp declare reduction (baz : integer : sub8 (omp_out, omp_in)) & ! { dg-error "Rank mismatch in argument\[^\n\r]*rank-1 and scalar" }
!$omp initializer (omp_priv = fn8 (omp_orig)) ! { dg-error "Rank mismatch in argument\[^\n\r]*rank-1 and scalar|Incompatible ranks 0 and 1 in assignment" }
  integer :: a
!$omp parallel reduction (foo : a)
!$omp end parallel
!$omp parallel reduction (bar : a)
!$omp end parallel
!$omp parallel reduction (baz : a)
!$omp end parallel
end subroutine test15
subroutine test16
  use m
!$omp declare reduction (foo : integer : omp_out = omp_out + omp_in) initializer (omp_priv = 0)
!$omp declare reduction (bar : integer : omp_out = fn7 (omp_out, omp_in)) &
!$omp & initializer (sub7 (omp_priv, omp_orig))
!$omp declare reduction (baz : integer : sub8 (omp_out, omp_in)) &
!$omp initializer (omp_priv = fn8 (omp_orig))
  integer, allocatable :: a(:)
  allocate (a(10))
!$omp parallel reduction (foo : a)
!$omp end parallel
!$omp parallel reduction (bar : a)
!$omp end parallel
!$omp parallel reduction (baz : a)
!$omp end parallel
end subroutine test16
subroutine test17
  use m
!$omp declare reduction (foo : integer : omp_out = omp_out + omp_in) initializer (omp_priv = 0)
!$omp declare reduction (bar : integer : omp_out = fn7 (omp_out, omp_in)) & ! { dg-error "Rank mismatch in argument\[^\n\r]*rank-1 and scalar|Incompatible ranks 0 and 1 in assignment" }
!$omp & initializer (sub7 (omp_priv, omp_orig)) ! { dg-error "Rank mismatch in argument\[^\n\r]*rank-1 and scalar" }
!$omp declare reduction (baz : integer : sub8 (omp_out, omp_in)) & ! { dg-error "Rank mismatch in argument\[^\n\r]*rank-1 and scalar" }
!$omp initializer (omp_priv = fn8 (omp_orig)) ! { dg-error "Rank mismatch in argument\[^\n\r]*rank-1 and scalar|Incompatible ranks 0 and 1 in assignment" }
  integer, allocatable :: a
  allocate (a)
!$omp parallel reduction (foo : a)
!$omp end parallel
!$omp parallel reduction (bar : a)
!$omp end parallel
!$omp parallel reduction (baz : a)
!$omp end parallel
end subroutine test17
