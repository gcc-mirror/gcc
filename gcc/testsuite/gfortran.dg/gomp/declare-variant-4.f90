program main
  implicit none
contains
  function f6 (x, y, z)
    real (kind = 8) :: f6
    integer, intent(in) :: x
    integer (kind = 8), intent(in) :: y
    real (kind = 4), intent(in) :: z

    interface
      function f1 (x, y, z)
        real (kind = 8) :: f1
        integer, intent(in) :: x
        integer (kind = 8), intent(in) :: y
        real (kind = 4), intent(in) :: z
      end function

      function f2 (x, y, z)
        real (kind = 8) :: f2
        integer, intent(in) :: x
        integer (kind = 8), intent(in) :: y
        real (kind = 4), intent(in) :: z
      end function

      function f3 (x, y, z)
        real (kind = 8) :: f3
        integer, intent(in) :: x
        integer (kind = 8), intent(in) :: y
        real (kind = 4), intent(in) :: z
      end function

      function f4 (x, y, z)
        real (kind = 8) :: f4
        integer, intent(in) :: x
        integer (kind = 8), intent(in) :: y
        real (kind = 4), intent(in) :: z
      end function

      function f5 (x, y, z)
        real (kind = 8) :: f5
        integer, intent(in) :: x
        integer (kind = 8), intent(in) :: y
        real (kind = 4), intent(in) :: z
      end function
    end interface

    !$omp declare variant (f1) match (user={condition(1)})
    !$omp declare variant (f2) match (user={condition(score(1):1)})
    !$omp declare variant (f3) match (user={condition(score(3):1)})
    !$omp declare variant (f4) match (user={condition(score(2):1)})
    !$omp declare variant (f5) match (implementation={vendor(gnu)})

    f6 = z + x + y
  end function

  function test (x)
    real (kind = 8) :: test
    integer, intent(in) :: x

    test = f6 (x, int (x, kind = 8), 3.5)
  end function
end program
