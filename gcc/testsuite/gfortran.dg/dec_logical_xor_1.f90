! { dg-do run }
! { dg-options "-std=legacy" }
!
! Test logical .XOR. operator.
!

implicit none

logical :: in1, in2, neqv_out, lxor_out, truth_table(2)
integer :: i, j, ixor_out, ieor_out

truth_table(1) = .true.
truth_table(2) = .false.
do i = 1,2
  do j = 1,2
    in1 = truth_table(j)
    in2 = truth_table(i)

    ! make sure logical xor works
    neqv_out = in1 .neqv. in2
    lxor_out = in1 .xor. in2

    if ( neqv_out .neqv. lxor_out ) then
      print *, "(",in1,in2,") .neqv.: ",neqv_out,"  .xor.: ",lxor_out
      STOP 1
    endif

    ! make sure we didn't break xor() intrinsic
    ixor_out = xor(i*7, j*5)
    ieor_out = ieor(i*7, j*5)

    if ( ixor_out .ne. ieor_out ) then
      print *, "(",in1,in2,") ieor(): ",ieor_out,"  xor(): ",ixor_out
      STOP 2
    endif

  enddo
enddo

end
