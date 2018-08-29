! PR rtl-optimization/82973
! { dg-do compile }
! { dg-options "-Ofast -frounding-math" }

program pr82973
  integer, parameter :: n=16
  real, dimension(n) :: ar, br, modulo_result, floor_result
  integer, dimension(n) :: ai, bi , imodulo_result, ifloor_result
  ai(1:4) = 5
  ai(5:8) = -5
  ai(9:12) = 1
  ai(13:16) = -1
  bi(1:4) = (/ 3,-3, 1, -1/)
  bi(5:8) = bi(1:4)
  bi(9:12) = bi(1:4)
  bi(13:16) = bi(1:4)
  ar = ai
  br = bi
  modulo_result = modulo(ar,br)
  imodulo_result = modulo(ai,bi)
  floor_result = ar-floor(ar/br)*br
  ifloor_result = nint(real(ai-floor(real(ai)/real(bi))*bi))
  do i=1,n
    if (modulo_result(i) /= floor_result(i)) then
      STOP 1
    end if
    if (imodulo_result(i) /= ifloor_result(i)) then
      STOP 2
    end if
  end do
end program pr82973
