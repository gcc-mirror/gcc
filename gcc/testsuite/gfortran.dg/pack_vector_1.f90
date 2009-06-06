! { dg-do "compile" }
!
! Check that the VECTOR argument of the PACK intrinsic has at least
! as many elements as the MASK has .TRUE. values.
!

  INTEGER :: res(2)
  res = PACK ((/ 1, 2, 3 /), (/.TRUE., .TRUE., .FALSE. /), SHAPE(1)) !{ dg-error "must provide at least as many" }
  res = PACK ((/ 1, 2, 3 /), (/.TRUE., .TRUE., .FALSE. /), (/ -1 /)) !{ dg-error "must provide at least as many" }
END
