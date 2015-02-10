! { dg-do run }
! PR58722
program testit
  character(50) :: astring
  
  write(astring, '(g0)') 0.1_4
  if (test(astring)) call abort
  write(astring, '(g0)') 0.1_8
  if (test(astring)) call abort
  write(astring, '(g0)') 0.1_10
  if (test(astring)) call abort
  write(astring, '(g0)') 0.1_16
  if (test(astring)) call abort

contains

function test (string1) result(res)
  character(len=*) :: string1
  logical :: res

  res = .true.
  do i = 1, len(string1)
    if (string1(i:i) == 'E') return
  end do
  res = .false.
end function

end program
