! { dg-do run }
! PR fortran/96896

call test1
call reshape_test
end

subroutine test1
implicit none
integer, target :: B
integer, pointer :: A(:)
allocate(A(5))
A = 1
B = 10
get_A() = get_B()
if (any (A /= 10)) stop 1
get_A() = get_A()
if (any (A /= 10)) stop 2
deallocate(A)
contains
  function get_A()
    integer, pointer :: get_A(:)
    get_A => A
  end
  function get_B()
    integer, pointer :: get_B
    get_B => B
  end
end

subroutine reshape_test
    implicit none
    real, target, dimension (1:9) :: b
    integer :: i
    b = 1.0
    myshape(b) = 3.0
    do i = 1, 3
      myfunc (b,i,2) = b(i) + i
      b(i) = b(i) + 2.0
    end do
    if (any (b /= [real::5,5,5,4,5,6,3,3,3])) stop 3
contains
  function myfunc(b,i,j)
    real, target, dimension (1:9) :: b
    real, pointer :: myfunc
    real, pointer :: p(:,:)
    integer :: i,j 
    p => myshape(b)
    myfunc => p(i,j)
  end function myfunc
  function myshape(b)
    real, target, dimension (1:9) :: b
    real, pointer :: myshape(:,:)
    myshape(1:3,1:3) => b
  end function myshape
end subroutine reshape_test
