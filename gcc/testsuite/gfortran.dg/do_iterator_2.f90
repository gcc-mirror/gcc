! { dg-do run }
! Tests the fix for pr32613 - see:
! http://groups.google.com/group/comp.lang.fortran/browse_thread/thread/495c154ee188d7f1/ea292134fe68b1d0#ea292134fe68b1d0
!
! Contributed by Al Greynolds <awgreynolds@earthlink.net>
!
program main
  call something
end

subroutine something
!  integer i !correct results from gfortran depend on this statement (before fix)
  integer :: m = 0
  character lit*1, line*100
  lit(i) = line(i:i)
  i = 1
  n = 5
  line = 'PZ0R1'
  if (internal (1)) call abort ()
  if (m .ne. 4) call abort ()
contains
  logical function internal (j)
    intent(in) j
    do i = j, n
      k = index ('RE', lit (i))
      m = m + 1
      if (k == 0) cycle
      if (i + 1 == n) exit
    enddo
    internal = (k == 0)
  end function
end
