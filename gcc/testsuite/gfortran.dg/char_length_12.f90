! { dg-do run }

  implicit none
  character(len=3), dimension(3,3), parameter :: &
    p = reshape(["xyz", "abc", "mkl", "vpn", "lsd", "epo", "tgv", &
                 "bbc", "wto"], [3,3])
  character(len=3), dimension(3,3) :: m1

  m1 = p
  if (any (spread (p, 1, 2) /= spread (m1, 1, 2))) call abort

end
