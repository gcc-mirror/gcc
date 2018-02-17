! { dg-do run }
module nonF03ComBlock
  common /NONF03COM/ r, s
  real :: r
  real :: s
  
  contains
    
    subroutine hello(myArray)
      integer, dimension(:) :: myArray

      r = 1.0
      s = 2.0
    end subroutine hello
end module nonF03ComBlock

program testComBlock
  use nonF03ComBlock
  integer, dimension(1:10) :: myArray

  call hello(myArray)

  ! these are set in the call to hello() above
  ! r and s are reals (default size) in com block, set to 
  ! 1.0 and 2.0, respectively, in hello()
  if(r .ne. 1.0) then 
     STOP 1
  endif
  if(s .ne. 2.0) then
     STOP 2
  endif
end program testComBlock
