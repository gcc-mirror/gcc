! { dg-do  run }
! PR 66113 - this used to ICE with deeply nested BLOCKS.
program main
  integer :: n
  real :: s
  n = 3
  block
    block
      block
        block
          block
            real, dimension(n) :: a
            a = 3.
            s = sum(a)
          end block
        end block
      end block
    end block
  end block
  if (s /= 9) STOP 1
end program main
