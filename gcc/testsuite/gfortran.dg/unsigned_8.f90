! { dg-do run }
! { dg-options "-funsigned" }
! Test bit_size, btest and bgt plus friends.
program main
  implicit none
  unsigned :: u
  integer :: i, j
  unsigned :: ui, uj
  logical:: test_i, test_u
  if (bit_size(u) /= 32) error stop 1
  if (.not. btest(32,5)) error stop 2
  if (btest(32,4)) error stop 3
  u = 32u
  if (btest(u,4)) error stop 4
  do i=1,3
     ui = uint(i)
     do j=1,3
        uj = uint(j)
        test_i = blt(i,j)
        test_u = blt(ui,uj)
        if (test_i .neqv. test_u) error stop 5
        test_i = ble(i,j)
        test_u = ble(ui,uj)
        if (test_i .neqv. test_u) error stop 6
        test_i = bge(i,j)
        test_u = bge(ui,uj)
        if (test_i .neqv. test_u) error stop 7
        test_i = bgt(i,j)
        test_u = bgt(ui,uj)
        if (test_i .neqv. test_u) error stop 8
     end do
  end do
  if (blt (1, 1) .neqv. blt (1u, 1u)) error stop 8
  if (ble (1, 1) .neqv. ble (1u, 1u)) error stop 9
  if (bge (1, 1) .neqv. bge (1u, 1u)) error stop 10
  if (bgt (1, 1) .neqv. bgt (1u, 1u)) error stop 11
  if (blt (1, 2) .neqv. blt (1u, 2u)) error stop 12
  if (ble (1, 2) .neqv. ble (1u, 2u)) error stop 13
  if (bge (1, 2) .neqv. bge (1u, 2u)) error stop 14
  if (bgt (1, 2) .neqv. bgt (1u, 2u)) error stop 15
  if (blt (1, 3) .neqv. blt (1u, 3u)) error stop 16
  if (ble (1, 3) .neqv. ble (1u, 3u)) error stop 17
  if (bge (1, 3) .neqv. bge (1u, 3u)) error stop 18
  if (bgt (1, 3) .neqv. bgt (1u, 3u)) error stop 19
  if (blt (2, 1) .neqv. blt (2u, 1u)) error stop 20
  if (ble (2, 1) .neqv. ble (2u, 1u)) error stop 21
  if (bge (2, 1) .neqv. bge (2u, 1u)) error stop 22
  if (bgt (2, 1) .neqv. bgt (2u, 1u)) error stop 23
  if (blt (2, 2) .neqv. blt (2u, 2u)) error stop 24
  if (ble (2, 2) .neqv. ble (2u, 2u)) error stop 25
  if (bge (2, 2) .neqv. bge (2u, 2u)) error stop 26
  if (bgt (2, 2) .neqv. bgt (2u, 2u)) error stop 27
  if (blt (2, 3) .neqv. blt (2u, 3u)) error stop 28
  if (ble (2, 3) .neqv. ble (2u, 3u)) error stop 29
  if (bge (2, 3) .neqv. bge (2u, 3u)) error stop 30
  if (bgt (2, 3) .neqv. bgt (2u, 3u)) error stop 31
  if (blt (3, 1) .neqv. blt (3u, 1u)) error stop 32
  if (ble (3, 1) .neqv. ble (3u, 1u)) error stop 33
  if (bge (3, 1) .neqv. bge (3u, 1u)) error stop 34
  if (bgt (3, 1) .neqv. bgt (3u, 1u)) error stop 35
  if (blt (3, 2) .neqv. blt (3u, 2u)) error stop 36
  if (ble (3, 2) .neqv. ble (3u, 2u)) error stop 37
  if (bge (3, 2) .neqv. bge (3u, 2u)) error stop 38
  if (bgt (3, 2) .neqv. bgt (3u, 2u)) error stop 39
  if (blt (3, 3) .neqv. blt (3u, 3u)) error stop 40
  if (ble (3, 3) .neqv. ble (3u, 3u)) error stop 41
  if (bge (3, 3) .neqv. bge (3u, 3u)) error stop 42
  if (bgt (3, 3) .neqv. bgt (3u, 3u)) error stop 43

end
