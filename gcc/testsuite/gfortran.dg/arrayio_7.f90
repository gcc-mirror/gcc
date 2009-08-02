! { dg-do run }
! { dg-options "-std=legacy" }
!
! PR24224 Test formatted input/output to/from character arrays with strides
! other than 1.  Test that reading stops at the end of the current record.
! Contributed by Jerry DeLisle <jvdelisle@verizon.net>.
program arrayio_7
  character*4, dimension(8) :: abuf = (/"0123","4567","89AB","CDEF", &
                                        "0123","4567","89AB","CDEF"/)
  character*4, dimension(2,4) :: buf
  character*8 :: a
  equivalence (buf,abuf)
  read(buf(2, 1:3:2), '(a8)') a
  if (a.ne."4567") call abort()
end program arrayio_7
