! { dg-do compile }
! { dg-options "-Warray-temporaries -fdump-tree-original" }
!
! PR fortran/45648
! Non-copying descriptor transpose optimization (for function call args).
!
! Contributed by Richard Sandiford <richard@codesourcery.com>

module foo
  interface
    subroutine ext1 (a, b)
      real, intent (in), dimension (:, :) :: a, b
    end subroutine ext1
    subroutine ext2 (a, b)
      real, intent (in), dimension (:, :) :: a
      real, intent (out), dimension (:, :) :: b
    end subroutine ext2
    subroutine ext3 (a, b)
      real, dimension (:, :) :: a, b
    end subroutine ext3
  end interface
contains
  ! No temporary needed here.
  subroutine test1 (n, a, b, c)
    integer :: n
    real, dimension (n, n) :: a, b, c
    a = matmul (transpose (b), c)
  end subroutine test1

  ! No temporary either, as we know the arguments to matmul are intent(in)
  subroutine test2 (n, a, b)
    integer :: n
    real, dimension (n, n) :: a, b
    a = matmul (transpose (b), b)
  end subroutine test2

  ! No temporary needed.
  subroutine test3 (n, a, b, c)
    integer :: n
    real, dimension (n, n) :: a, c
    real, dimension (n+4, n+4) :: b
    a = matmul (transpose (b (2:n+1, 3:n+2)), c)
  end subroutine test3

  ! A temporary is needed for the result of either the transpose or matmul.
  subroutine test4 (n, a, b)
    integer :: n
    real, dimension (n, n) :: a, b
    a = matmul (transpose (a), b)       ! { dg-warning "Creating array temporary" }
  end subroutine test4

  ! The temporary is needed here since the second argument to imp1
  ! has unknown intent.
  subroutine test5 (n, a)
    integer :: n
    real, dimension (n, n) :: a
    call imp1 (transpose (a), a)        ! { dg-warning "Creating array temporary" }
  end subroutine test5

  ! No temporaries are needed here; imp1 can't modify either argument.
  ! We have to pack the arguments, however. 
  subroutine test6 (n, a, b)
    integer :: n
    real, dimension (n, n) :: a, b
    call imp1 (transpose (a), transpose (b))    ! { dg-warning "Creating array temporary" }
  end subroutine test6

  ! No temporaries are needed here; imp1 can't modify either argument.
  ! We don't have to pack the arguments. 
  subroutine test6_bis (n, a, b)
    integer :: n
    real, dimension (n, n) :: a, b
    call ext3 (transpose (a), transpose (b))
  end subroutine test6_bis

  ! No temporary is neede here; the second argument is intent(in).
  subroutine test7 (n, a)
    integer :: n
    real, dimension (n, n) :: a
    call ext1 (transpose (a), a)
  end subroutine test7

  ! The temporary is needed here though.
  subroutine test8 (n, a)
    integer :: n
    real, dimension (n, n) :: a
    call ext2 (transpose (a), a)        ! { dg-warning "Creating array temporary" } 
  end subroutine test8

  ! Silly, but we don't need any temporaries here.
  subroutine test9 (n, a)
    integer :: n
    real, dimension (n, n) :: a
    call ext1 (transpose (transpose (a)), a)
  end subroutine test9

  ! The outer transpose needs a temporary; the inner one doesn't.
  subroutine test10 (n, a)
    integer :: n
    real, dimension (n, n) :: a
    call ext2 (transpose (transpose (a)), a)    ! { dg-warning "Creating array temporary" }
  end subroutine test10
end module foo

! { dg-final { scan-tree-dump-times "struct\[^\\n\]*atmp" 4 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
! { dg-final { cleanup-modules "foo" } }
