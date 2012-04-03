! { dg-do compile }
! { dg-options "-O3" }
function pr52701 (x, z, e, f, g, l)
  integer a, b, c, d, e, f, g, i, j, l, pr52701
  double precision x(e), z(e*e)
  do i = l, f
    do j = l, i
      d = 0
      do a = 1, g
        c = a - g
        do b = 1, g
          d = d + 1
          c = c + g
          z(d) = z(d) / (x(i) + x(j) - x(f + a) - x(f + b))
        end do
      end do
    end do
  end do
  pr52701 = c
end
