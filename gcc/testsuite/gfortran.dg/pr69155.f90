! PR tree-optimization/69155
! { dg-do compile }

function pr69155 (a, b)
  complex(kind=8), value :: a, b
  if (dimag (a) .lt. 10) then
  1 continue
    if (dble (a) .lt. 10) then
      b = b - 1 / a
      a = a + 1
      goto 1
    end if
  end if
  pr69155 = a + b
end
