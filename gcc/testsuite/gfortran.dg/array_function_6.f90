! { dg-do run }
! PR46842 wrong results with MATMUL(..., TRANSPOSE (func ()))
implicit none
call sub()
contains
   subroutine sub()
      real, dimension(2,2) :: b
      b = 1.0
      b = matmul(b,transpose(func()))
      if (any(b.ne.reshape((/ 4.0, 4.0, 6.0, 6.0 /),[2,2]) )) print *, b
   end subroutine

   function func() result(res)
      real, dimension(2,2) :: res
      res = reshape([1,2,3,4], [2,2])
   end function
end
