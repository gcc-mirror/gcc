! { dg-do run }
! { dg-options "-std=legacy" }
!
! PR24489 Assure that read does not go past the end of record. The width of
! the format specifier is 8, but the internal unit record length is 4 so only
! the first 4 characters should be read.
! Contributed by Jerry DeLisle <jvdelisle@verizon.net>.
program pr24489
  character*4, dimension(8) :: abuf = (/"0123","4567","89AB","CDEF", &
                                        "0123","4567","89AB","CDEF"/)
  character*4, dimension(2,4) :: buf
  character*8 :: a
  equivalence (buf,abuf)
  read(buf, '(a8)') a
  if (a.ne.'0123') call abort()
end program pr24489
