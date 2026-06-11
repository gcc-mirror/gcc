! { dg-do run }
!
! PR 125747
! Three consecutive DO CONCURRENT constructs reuse the index-name 'i'
! with type-specs.  The second and third constructs each need a shadow
! variable per F2018 19.4(6); they used to be assigned the same shadow
! symbol "_i", and the third construct's INTEGER(2) type-spec mutated
! the shadow symbol shared with the second construct, leaving its
! already-converted INTEGER(4) bounds inconsistent with the (now
! INTEGER(2)) loop variable and causing an ICE in gfc_add_modify_loc.

program p
  implicit none
  integer :: s1, s2, s3

  s1 = 0
  do concurrent (integer :: i = 1:3) reduce(+:s1)
    s1 = s1 + i
  end do

  s2 = 0
  do concurrent (integer :: i = 1:3) reduce(+:s2)
    s2 = s2 + i
  end do

  s3 = 0
  do concurrent (integer(2) :: i = 1:3) reduce(+:s3)
    s3 = s3 + int (i, kind (s3))
  end do

  if (s1 /= 6 .or. s2 /= 6 .or. s3 /= 6) stop 1
end program p
