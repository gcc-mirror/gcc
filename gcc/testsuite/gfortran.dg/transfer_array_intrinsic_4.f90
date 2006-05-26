! { dg-do run }
! { dg-options "-std=gnu" }
! Tests patch for pr27155, where character scalar string_lengths
! were not correctly translated by the array transfer intrinsic.
!
! Contributed by Bo Berggren  <bo.berggren@glocalnet.net>
!
program trf_test
      implicit none
      character(11) :: s1, s2
      integer(4) :: ia(3)
      integer(1) :: ba(12)
      equivalence (ia, ba)

      s1 = 'ABCDEFGHIJK'
      ia = TRANSFER (s1, (/ 0_4 /))
      s2 = TRANSFER(ba + 32_1, s2)

      if (s2 .ne. 'abcdefghijk') call abort ()

      s1 = 'AB'
      ba = TRANSFER (trim (s1)//'       JK' , (/ 0_1 /))
      s2 = TRANSFER(ia, s2)

      if (trim (s1)//'       JK' .ne. s2) call abort ()

end program trf_test
