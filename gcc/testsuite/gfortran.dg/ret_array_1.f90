! { dg-do run }
! Test functions returning arrays of indeterminate size.
program ret_array_1
  integer, dimension(:, :), allocatable :: a
  integer, dimension(2) :: b

  allocate (a(2, 3))
  a = reshape ((/1, 2, 3, 4, 5, 6/), (/2, 3/))
  
  ! Using the return value as an actual argument
  b = 0;
  b = sum (transpose (a), 1);
  if (any (b .ne. (/9, 12/))) call abort ()

  ! Using the return value in an expression
  b = 0;
  b = sum (transpose (a) + 1, 1);
  if (any (b .ne. (/12, 15/))) call abort ()

  ! Same again testing a user function
! TODO: enable these once this is implemented
!  b = 0;
!  b = sum (my_transpose (a), 1);
!  if (any (b .ne. (/9, 12/))) call abort ()
!
!  ! Using the return value in an expression
!  b = 0;
!  b = sum (my_transpose (a) + 1, 1);
!  if (any (b .ne. (/12, 15/))) call abort ()
contains
subroutine test(x, n)
  integer, dimension (:, :) :: x
  integer n

  if (any (shape (x) .ne. (/3, 2/))) call abort
  if (any (x .ne. (n + reshape((/1, 4, 2, 5, 3, 6/), (/3, 2/))))) call abort
end subroutine

function my_transpose (x) result (r)
  interface
    pure function obfuscate (i)
      integer obfuscate
      integer, intent(in) :: i
    end function
  end interface
  integer, dimension (:, :) :: x
  integer, dimension (obfuscate(ubound(x, 2)), &
                      obfuscate(ubound(x, 1))) :: r
  integer i

  do i = 1, ubound(x, 1)
    r(:, i) = x(i, :)
  end do
end function
end program

pure function obfuscate (i)
  integer obfuscate
  integer, intent(in) :: i

  obfuscate = i
end function

