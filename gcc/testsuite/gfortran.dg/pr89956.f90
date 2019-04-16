! { dg-options "-O3 -fno-tree-forwprop -fno-tree-pre -fno-tree-dominator-opts -fno-code-hoisting -ffast-math" }

module de
contains
  function zu (az, xx) result (q3)
    real :: az, xx, q3

    q3 = 1.0 - lz (az, xx) - lz (xx, az)
  end function zu

  function lz (ho, gh) result (ye)
    real :: ho, gh, ye

    ye = sqrt (ho) - ho * gh
  end function lz
end module de
