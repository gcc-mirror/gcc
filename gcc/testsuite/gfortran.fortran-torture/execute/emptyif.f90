! Test empty if statements.  We Used to fail this because we folded
! the if stmt before we finished building it.
program emptyif
  implicit none
  integer i
  
  i=1
  if(i .le. 0) then
  else
    i = 2
  endif
  if (i .ne. 2) call abort()

  if (i .eq. 0) then
  elseif (i .eq. 2) then
    i = 3
  end if
  if (i .ne. 3) call abort()
end

