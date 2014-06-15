! { dg-do run }

  integer :: i, j, k
  integer(kind=8) :: i8, j8, k8
  real :: x
  double precision :: z

  call system_clock(i, j, k)
  call system_clock(i, j, k8)
  call system_clock(i, j8, k)
  call system_clock(i, j8, k8)
  call system_clock(i8, j, k)
  call system_clock(i8, j, k8)
  call system_clock(i8, j8, k)
  call system_clock(i8, j8, k8)

  call system_clock(i, x, k)
  call system_clock(i, x, k8)
  call system_clock(i, x, k)
  call system_clock(i, x, k8)
  call system_clock(i8, x, k)
  call system_clock(i8, x, k8)
  call system_clock(i8, x, k)
  call system_clock(i8, x, k8)

  call system_clock(i, z, k)
  call system_clock(i, z, k8)
  call system_clock(i, z, k)
  call system_clock(i, z, k8)
  call system_clock(i8, z, k)
  call system_clock(i8, z, k8)
  call system_clock(i8, z, k)
  call system_clock(i8, z, k8)

  end
