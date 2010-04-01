! { dg-do run }
! PR43605 FTELL intrinsic returns incorrect position
! Contributed by Janne Blomqvist and Manfred Schwarb
program ftell_3
  integer :: i
  character(len=99) :: buffer
  open(10, form='formatted', status='scratch', position='rewind')
  write(10, '(a)') '123456'
  write(10, '(a)') '789'
  write(10, '(a)') 'CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC'
  write(10, '(a)') 'DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD'
  rewind(10)
  read(10, '(a)') buffer
  call ftell(10, i)
  if(i /= 7) then
     call abort()
  end if
  close(10)
end program ftell_3
