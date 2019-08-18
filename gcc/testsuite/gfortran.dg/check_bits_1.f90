! { dg-do run }
! { dg-options "-fcheck=bits -fdump-tree-original" }
! { dg-shouldfail "Fortran runtime error: SIZE argument (0) out of range 1:32 in intrinsic ISHFTC" }
! { dg-output "At line 44 .*" }
!
! Verify that the runtime checks for the bit manipulation intrinsic functions
! do not generate false-positives
program check
  implicit none
  integer :: i, k, pos, len, shift, size, nb
  nb = bit_size (i)
  i = 0
  do pos = 0, nb-1
     k = ibset (i, pos)
     i = ibclr (k, pos)
     if (btest (i, pos)) stop 1
  end do
  do pos = 0, nb
     do len = 0, nb-pos
        i = ibits (i, pos, len)
     end do
  end do
  do shift = 0, nb
     k = ishft (i,  shift)
     i = ishft (k, -shift)
  end do
  do shift = 0, nb
     k = shiftl (i, shift) ! Fortran 2008
     i = shiftr (k, shift)
     i = shifta (i, shift)
     k = lshift (i, shift) ! GNU extensions
     i = rshift (k, shift)
  end do
  do shift = 0, nb
     k = ishftc (i,  shift)
     i = ishftc (k, -shift)
     do size = max (1,shift), nb
        k = ishftc (i,  shift, size)
        i = ishftc (k, -shift, size)
     end do
  end do
  size = 0
  ! The following line should fail with a runtime error:
  k = ishftc (i, 0, size)
  ! Should never get here with -fcheck=bits
  stop 2
end program check

! { dg-final { scan-tree-dump-times "_gfortran_runtime_error_at" 21 "original" } }
